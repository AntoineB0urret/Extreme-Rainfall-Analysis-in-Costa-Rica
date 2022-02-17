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

class simpleGEVloss(nn.Module):
    def __init__(self):
        super(simpleGEVloss, self).__init__()
        
    def forward(self, x, observed_values):
        """ x: [location, scale, shape],
        observed_values: [intensities, duration] """
        location = x[:, 0]
        scale = x[:, 1]
        shape = x[:, 2]
        term1 = torch.log(scale)
        term_inter_1 = 1.0 + shape * (observed_values[:, 0] - location)/scale
        term_inter_1[term_inter_1 <= 0] = 1e-16
        term3 = (1.0/shape + 1.0)* torch.log(term_inter_1)
        term4 = term_inter_1 ** (-1/shape)

        indices_small_shape = torch.abs(shape) < 1e-5
        term3 = 1.0*indices_small_shape * term3 + (1.0-1.0*indices_small_shape)*(observed_values[:, 0] - location)/scale
        term4 = 1.0*indices_small_shape * term4 + (1.0-1.0*indices_small_shape)*torch.exp(-(observed_values[:, 0] - location)/scale)

        res = term1 + term3 + term4
        return torch.sum(res)