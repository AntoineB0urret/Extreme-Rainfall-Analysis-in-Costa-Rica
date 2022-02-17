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

# ==============================[ Data loaders ]============================== #
class BaseDataLoader():
    def __init__(self, dataInput, dataOutput, batch_size, maxIterations = None):
        self.batch_size = batch_size
        self.count = 0
        self.nb_epoch = 0
        self.dataInput = dataInput
        self.dataOutput = dataOutput
        self.maxIterations = maxIterations
        if self.maxIterations is None:
            self.maxIterations = np.Inf
    
    def __iter__(self):
        return self
    
    def __next__(self):
        self.count += 1
        if self.count > self.maxIterations:
            raise StopIteration
        return self.sample()
    
    def sample(self):
        pass
    
    def reset(self):
        self.count = 0

class DataLoader2(BaseDataLoader):
    """ Data loader for neural network P1+P2
    
    Expecting inputs of the form:
    dataInput -> duration, Jan, Feb, ..., Dec
    dataOutput -> int
    
    """
    def __init__(self, dataInput, dataOutput, batch_size, maxIterations = None):
        super(DataLoader2, self).__init__(dataInput, dataOutput, batch_size, maxIterations)
    
    def sample(self):
        chosen_data = np.random.choice(self.dataInput.shape[0], size = self.batch_size, replace = True)
        input_batch = self.dataInput[chosen_data, :]
        output_batch = torch.cat((self.dataOutput[chosen_data].unsqueeze(dim = 1),
                                  input_batch[:, 0].unsqueeze(dim = 1)
                                 ), axis=1)
        return {'input': input_batch, 'output': output_batch}

    def getAlldata(self):
        input_batch = self.dataInput
        output_batch = torch.cat((self.dataOutput.unsqueeze(dim = 1),
                                  input_batch[:, 0].unsqueeze(dim = 1)
                                 ), axis=1)
        return {'input': input_batch, 'output': output_batch}

    def getSample(self, size = 1):
        chosen_data = np.random.choice(self.dataInput.shape[0], size = size, replace = True)
        input_batch = self.dataInput[chosen_data, :]
        output_batch = torch.cat((self.dataOutput[chosen_data].unsqueeze(dim = 1),
                                  input_batch[:, 0].unsqueeze(dim = 1)
                                 ), axis=1)
        return {'input': input_batch, 'output': output_batch}