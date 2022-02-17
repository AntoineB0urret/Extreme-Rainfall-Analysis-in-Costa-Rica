import numpy as np
import pandas as pd
import scipy as sc
import torch
import matplotlib.pyplot as plt
from tqdm import tqdm

import math
import time
import numpy as np

import torch
import torch.nn as nn
from torch.nn import LSTM
import torch.nn.functional as F

from torch.autograd import Variable
from torch.optim.lr_scheduler import _LRScheduler
from torch.optim.optimizer import Optimizer

# ==============================[ training functions ]============================== #
import shutil
import os

class NeuralTrainer(object):

    def __init__(self, 
                 model,                 # model to be trained
                 criterion,             # loss function
                 optimizer,             # optimizer to update the parameters
                 dataloaders_dict,      # dictionary of dataloaders
                 diagnostic,            # diagnostic class: record training and testing errors
                 scheduler = None,      # scheduler, to change the optimizer's parameters
                 # early_stop_handler = None,
                 path_save_model = None,# location where the model will be saved
                 saveFrequency = None,  # frequency by which the model is saved
                 best_loss = None,      # best loss achieved (not necessarily given)
                 model_name = "",       # name of the model, for save
                 referenceSaveModel = "TESTING",  # name of the dataloader of reference for saving the model 
                 sampleTest = 0,        # size of the sample to be extracted during the learning. Can be 0.
                 recordFrequencySampleTest = 20,   # frequency at which samples are extracted
                 progressBar = False):  # True to show the progress bar
        
        self.model = model    # model to be trained
        self.criterion = criterion  # loss
        self.optimizer = optimizer  # optimizer to update the parameters
        self.dataloaders_dict = dataloaders_dict  # dictionary of dataloaders (for training and testing)
        self.diagnostic = diagnostic   # diagnostic class: record training and testing errors
        self.scheduler = scheduler   # scheduler, to change the optimizer's parameters
        self.path_save_model = path_save_model  # location where the model will be saved
        self.saveFrequency = saveFrequency
        self.best_loss = best_loss
        if self.best_loss is None:
            self.best_loss = float('inf')
        self.model_name = model_name
        self.referenceSaveModel = referenceSaveModel

        self.sampleTest = sampleTest
        self.recordFrequencySampleTest = recordFrequencySampleTest

        self.progressBar = progressBar

        self.test_lr = False # to find the best learning rate, should be found with hyperparameter search engines

        # self.early_stop_handler = early_stop_handler

        
    def trainModel_regular(self, number_of_epochs, training_step = None, validation_step = None, *args, **kwargs):

        if self.sampleTest > 0:
            dfs = {name: [None for j in range(0, self.sampleTest)] for name, dataloader in self.dataloaders_dict.items()}
            # dfs_truth = {name: [None for j in range(0, self.sampleTest)] for name, dataloader in self.dataloaders_dict.items()}
            list_samples = {name: [dataloader.getSample(size = 1) for j in range(0, self.sampleTest)] for name, dataloader in self.dataloaders_dict.items()}

        # iterate over all epochs
        # for epoch_idx in range(0, number_of_epochs):
        progressBar_custom, can_update_progress_bar = self.getIteratorMain(range(0, number_of_epochs))
        for epoch_idx in progressBar_custom:

            # train the batch of data for each dataloader (usually one for training, and one for testing), and record the losses
            losses = {name: training_step(dataloader, self.model, self.criterion, self.optimizer, name, epoch_idx/number_of_epochs, *args, **kwargs) for name, dataloader in self.dataloaders_dict.items()}


            # Update diagnostics with the current losses
            self.diagnostic.updateData({name: torch.tensor(np.sum(loss)) for name, loss in losses.items()}, model = self.model)


            # for each dataloader, reset the count (otherwise they raise StopIteration)
            for name, dataloader in self.dataloaders_dict.items():
                dataloader.reset()
                
            if self.scheduler is not None:
                self.scheduler.step()
                # self.scheduler.step(losses[self.referenceSaveModel])

            # if directory to save the model is specified as well as the frequency, then save the model
            if self.path_save_model is not None and self.saveFrequency is not None and epoch_idx % self.saveFrequency == 0:
                self.saveModel(epoch_idx, np.sum(losses[self.referenceSaveModel]))


            # tests: go over all samples, and get the estimates
            if self.sampleTest > 0 and epoch_idx % self.recordFrequencySampleTest == 0:
                for j in range(0, self.sampleTest):
                    # estimates of the j-th sample of each dataloader
                    results = {name: validation_step(dataloader, self.model, self.criterion, self.optimizer, data = list_samples[name][j], *args, **kwargs) for name, dataloader in self.dataloaders_dict.items()}
                    # print(results)

                    # for each dataloader, get the output of the model (estimates), and store them into a dataframe.
                    # if no dataframe, create one, else, append the estimate at the end
                    for name, dataloader in self.dataloaders_dict.items():
                        if dfs[name][j] is None:
                            dfs[name][j] = {epoch_idx: results[name]}
                        else:
                            dfs[name][j][epoch_idx] = results[name]

        
            if can_update_progress_bar:
                progressBar_custom.set_description("Losses: {}".format(losses))
                # print("Losses: {}".format(losses))

        # self.diagnostic.finalize(model = self.model, input_to_model = Variable(dataloader.sampleRegular()["input"].float()))  # close diagnostic plots (for example flush tensorboard and close it)
        self.diagnostic.finalize()

        if self.sampleTest > 0:
            return {"estimates": dfs, "inputs":list_samples}

        
    def saveModel(self, epoch_index, current_loss):
        # print("Saving model...")
        stateCheckpoint = {
            'epoch': epoch_index,
            'arch': self.model_name,
            'state_dict': self.model.state_dict(),
            'best_loss': self.best_loss,
            'optimizer': self.optimizer.state_dict()
        }
        # torch.save(stateCheckpoint, self.path_save_model + 'checkpoint.pth.tar')
        torch.save(stateCheckpoint, self.path_save_model.replace(".pth.tar", "_CHECKPOINT.pth.tar"))

        if current_loss < self.best_loss:
            self.best_loss = current_loss
            shutil.copyfile(self.path_save_model.replace(".pth.tar", "_CHECKPOINT.pth.tar"), self.path_save_model)

    def getIteratorMain(self, iterator):
        if self.progressBar:
            return tqdm(iterator), True
        else:
            return iterator, False



import matplotlib.pyplot as plt
import sys
# !{sys.executable} -m pip install plotly
import plotly.graph_objects as go
import plotly.express as px

class Diagnostic(object):
    """
    Class that shows errors as a function of the iterations.
    Parameters :
        __channels : labels of the data to be shown (list of strings)
        __length : total number of iterations
        __Y : data shown
        __fig : plot
        __lines : attribute of the plot
        __internal_state : keeps track of the number of iteration
        __path_figure : None for not saving the plot, otherwise a string for the name of the figure
    """

    def __init__(self, channels, length, path_figure = None, title = "Loss", xlabel = "Epoch", ylabel = "loss", useTensorBoard = False, pathTensorBoard = None):
        self.__channels = channels
        self.__length = length
        self.__Y = None
        self.__fig = None
        self.__lines = None
        self.__internal_state = 0
        self.__path_figure = path_figure
        self.__title = title
        self.__xlabel = xlabel
        self.__ylabel = ylabel

        self.__Y = torch.empty(len(self.__channels), self.__length).fill_(0)  # Matrix of data, full of 0

        self.useTensorBoard = useTensorBoard
        if useTensorBoard:
            if pathTensorBoard is None:
                pathTensorBoard = path_figure
            self.pathTensorBoard = pathTensorBoard
            self.writer = SummaryWriter(pathTensorBoard)


    def create_graph_plt(self):
        """ Creation of the graph """
        self.__Y = torch.empty(len(self.__channels), self.__length).fill_(0)  # Matrix of data, full of 0

        # plt.ion()
        plt.ioff()
        self.__fig = plt.figure()
        ax = self.__fig.add_subplot(111, label="ax1")
        self.__lines = []
        for j in torch.arange(len(self.__channels)):
            self.__lines.append(ax.plot(torch.arange(self.__length),
                                        self.__Y[j, :],
                                        linewidth=1,
                                        markersize=2,
                                        label=self.__channels[j]))

        plt.xlim([0, self.__length])
        plt.ylim([0, 1.5])
        plt.grid()
        plt.title(self.__title)
        plt.xlabel(self.__xlabel)
        plt.ylabel(self.__ylabel)

        return self.__Y

    def update_plt(self, updates):
        """ Updates the plot, using new informations of 'update', which is a list of length len(self.__channels) """
        if len(updates) != len(self.__channels):
            raise NameError("Sizes do not match !")

        # Updating the informations
        for j in torch.arange(len(self.__channels)):
            self.__Y[j, self.__internal_state] = updates[j]
            self.__lines[j][0].set_ydata(self.__Y[j, :])

        self.__fig.canvas.draw()
        self.__fig.canvas.flush_events()
        plt.pause(0.00001)
        plt.legend([self.__channels[j] + " {:0.3f}".format(self.__Y[j, self.__internal_state]) for j in torch.arange(len(self.__channels))], loc = 'best')

        self.__internal_state += 1  # incrementing number of iterations

        # saves the image if it's necessary
        if self.__internal_state == self.__length and self.__path_figure is not None:
            plt.savefig(self.__path_figure)

    def updateData(self, updates, model = None, tensorboard_same_plot_scalars = True, iteration = None):
        if len(list(updates.keys())) != len(self.__channels):
            raise NameError("Sizes do not match !")

        # Updating the informations
        for j in torch.arange(len(self.__channels)):
            self.__Y[j, self.__internal_state] = updates[list(updates.keys())[j]]


        if self.useTensorBoard:
            if iteration is None:
                iteration = self.__internal_state
            if tensorboard_same_plot_scalars:
                self.writer.add_scalars(f'losses', updates, iteration)
            else:
                for name_loss, loss in updates.items():
                    self.writer.add_scalar(name_loss, loss, iteration)

            if model is not None:
                for name_module, module in model.state_dict().items():
                    if name_module.endswith("weight") or name_module.endswith("bias"):
                        self.writer.add_histogram(name_module, module, iteration)

        self.__internal_state += 1  # incrementing number of iterations

    def create_graph_plotly(self):
        df_error = pd.DataFrame({channel: self.__Y[j, :] for j, channel in enumerate(self.__channels)})
        df_error.index = range(0, self.__internal_state)
        df_error.index.name = "Epoch"
        df_error = df_error.reset_index(level = 0)

        fig = px.line(df_error, x="Epoch", y=df_error.columns,
            hover_data={"Epoch": "|%B %d, %Y"},
            template="plotly_white"
        )
        # fig.update_xaxes(dtick="Y1",tickformat="%b\n%Y")

        fig.update_layout(
            title={'text': self.__title,'y':0.9,'x':0.5,'xanchor': 'center','yanchor': 'top'},
            xaxis_title=self.__xlabel,
            yaxis_title=self.__ylabel,
            legend_title="",
            updatemenus=[
            dict(
                 buttons=[
                     dict(label="Linear",  
                          method="relayout", 
                          args=[{"yaxis.type": "linear"}]),
                     dict(label="Log", 
                          method="relayout", 
                          args=[{"yaxis.type": "log"}]),
                                  ])]
        )
        if self.__path_figure is not None:
            fig.write_html(self.__path_figure, auto_open=False)
        else:
            fig.show()

    def get_data(self):
        return self.__Y

    def finalize(self, **kwargs):
        if self.useTensorBoard:
            if "model" in kwargs:
                self.writer.add_graph(**kwargs)
            self.writer.flush()
            self.writer.close()

            import os
            os.system('python -m tensorflow.tensorboard --logdir=' + self.pathTensorBoard)
