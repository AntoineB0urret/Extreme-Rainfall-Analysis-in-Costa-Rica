import numpy as np
import pandas as pd
import scipy as sc
import torch
import matplotlib.pyplot as plt


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

class ModuleCustom(nn.Module):
    def __init__(self):
        super(ModuleCustom, self).__init__()
        
    # ----------- training and testing modules -----------
    def train_batches(self, loader, model, criterion, optimizer, 
                      title = "TRAINING", iteration = None, parameters = None):
        model.train()
        total_loss = []  # to be changed to tensor / or use running mean instead

        # Iterate over the dataloader
        for batch_idx, data in enumerate(loader):

            if torch.cuda.is_available():
                # inputs = {key: None if dat_in is None else Variable(dat_in.float()).cuda() for key, dat_in in data['input'].items()}
                inputs = Variable(data['input'].float()).cuda()
                true_outputs = Variable(data['output'].float()).cuda()
                # true_outputs = {key: None if dat_out is None else Variable(dat_out.float()).cuda() for key, dat_out in data['output'].items()}
            else:
                # inputs = {key: None if dat_in is None else Variable(dat_in.float()) for key, dat_in in data['input'].items()}
                # true_outputs = {key: None if dat_out is None else Variable(dat_out.float()) for key, dat_out in data['output'].items()}
                inputs = Variable(data['input'].float())
                true_outputs = Variable(data['output'].float())

            # If in train mode...
            if title == "TRAINING":
                optimizer.zero_grad()
                # compute outputs
                model_output = model(inputs)
                loss = criterion(model_output.cpu(), true_outputs.cpu())

                with torch.autograd.set_detect_anomaly(True):
                # torch.nn.utils.clip_grad_norm_(model.parameters(), max_norm=1.0, norm_type=2.0)
                    loss.backward()   # backpropagation
                    # nn.utils.clip_grad_value_(model.parameters(), clip_value=1.0)
                    
                    def closure():
                        optimizer.zero_grad()
                        model_output = model(inputs)
                        loss = criterion(model_output, true_outputs)
                        loss.backward()
                        return loss
                    optimizer.step(closure)
                    total_loss.append(loss.data.cpu().numpy())

            else:   # if in validation mode, all is done with no gradient
                with torch.no_grad():
                    # compute outputs
                    model_output = model(inputs)
                    loss = criterion(model_output.cpu(), true_outputs.cpu())             
                    total_loss.append(loss.data.cpu().numpy())

        return np.mean(total_loss)

    def test_batches(self, loader, model, criterion, optimizer, 
                      title = "TRAINING", iteration = None, params = None, data = None, parameters = None):
        model.eval()

        if data is None:
            data = loader.getSample()

        if torch.cuda.is_available():
            inputs = Variable(data['input'].float()).cuda()
            if data['output'] is not None:
                true_outputs = Variable(data['output'].float()).cuda()
        else:
            inputs = Variable(data['input'].float())
            if data['output'] is not None:
                true_outputs = Variable(data['output'].float())

        with torch.no_grad():
            if params is not None:
                outputs = model(inputs, params = params)
            else:
                outputs = model(inputs)

        return {"input":data['input'], "true_value":data['output'], "output_model":outputs.cpu()}


class Model_unique_station(ModuleCustom):
  def __init__(self, linearBlock, 
    duration_theta_eta = False, 
    remove_first_input_dim = False):
    super(Model_unique_station, self).__init__()
    self.linearBlock = linearBlock
    self.duration_theta_eta = duration_theta_eta
    self.remove_first_input_dim = remove_first_input_dim

  def forward(self, input):
    # device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu")
    device = input.get_device()
    if self.remove_first_input_dim:
      x = self.linearBlock(input[:, 1:])
    else:
      x = self.linearBlock(input)
    x1 = torch.tensor([[1, 0, 0]], device = device) * x[:, 0:3]   # location: not constrained
    x2 = torch.tensor([[0, 1, 0]], device = device) * torch.exp(x[:, 0:3]+1e-5)     # scale: constrained in interval [1e-5, Inf]
    x3 = torch.tensor([[0, 0, 1]], device = device) * self.bounded_output(x[:, 0:3], -0.5, 1.0)        # shape: constrained in interval [-0.5, 1.0]
    y = x1+x2+x3

    if self.duration_theta_eta:
      y1 = torch.tensor([[1, 1, 0]], device = device) * y * (input[:,0].view(-1, 1) + torch.exp(x[:, 3]).view(-1, 1))**(-torch.sigmoid(x[:, 4]).view(-1, 1))
      y2 = torch.tensor([[0, 0, 1]], device = device) * y
      y = y1+y2

    # y = torch.tensor([[1, 1, 0]], device = device) * y * (input[:,0].view(-1, 1) + torch.exp(x[:, 3]).view(-1, 1))**(-torch.sigmoid(x[:, 4]).view(-1, 1)) + torch.tensor([[0, 0, 1]], device = device) * y
    return y

  def bounded_output(self, x, lower, upper):
    scale = upper - lower
    return scale * torch.sigmoid(x) + lower
