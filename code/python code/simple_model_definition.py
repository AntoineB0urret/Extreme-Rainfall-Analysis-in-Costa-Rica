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

from tqdm import tqdm


# ==============================[ basic neural network modules ]============================== #
class networkLinear(nn.Module):
    """ Simple feed-forward deep neural network, composed of several LinearBlock modules.
    :param nn.Sequential sequential: model
    """

    def __init__(self, input_size, output_size, numberLayers, intermediateLayers_size, activation = "PReLU", 
        batchnormalization = True, dropout=False, last_activation = None):
        super(networkLinear, self).__init__()

        if numberLayers >= 2:
            intermediateBlocks = [LinearBlock(intermediateLayers_size, intermediateLayers_size, activation, batchnormalization, dropout) for i in range(numberLayers) ]
            intermediateBlocks[0] = LinearBlock(input_size, intermediateLayers_size, activation, batchnormalization, dropout)
            if last_activation is None:
                last_activation = activation
            intermediateBlocks[-1] = LinearBlock(intermediateLayers_size, output_size, last_activation, batchnormalization, dropout)
        else:
            if last_activation is None:
                last_activation = activation

            intermediateBlocks = [LinearBlock(input_size, output_size, last_activation, batchnormalization, dropout)]

        self.sequential = nn.Sequential(*intermediateBlocks)

    def forward(self, input):
        """ Output of the model.
        :param torch.Tensor input: input tensor, of shape [batch size x input size]
        :returns: output of the model
        :rtype: torch.Tensor
        """
        output = self.sequential(input)
        return output


class LinearBlock(nn.Module):
    """ Linear block, composed of a linear layer, followed by activation, batchnormalization and dropout layers if requested.
    :param nn.Module activation: activation layer
    :param nn.Sequential linearBlock: nn module
    """
    def __init__(self, input_size, output_size, activation = "PReLU", batchnormalization = True, dropout=False):
        super(LinearBlock, self).__init__()

        # Activation type
        if activation == "ReLU":
            self.activation = nn.ReLU()
        elif activation == "PReLU":
            self.activation = nn.PReLU()  # the parameter for PReLU is learnable
        elif activation == "Sigmoid":
            self.activation = nn.Sigmoid()
        elif activation == "Tanh":
            self.activation = nn.Tanh()
        elif activation == "id":
            self.activation = identityNN()
        else:
            raise Exception('The activation function must be either \'ReLU\', \'PReLU\', \'Sigmoid\' or \'Tanh\'.')

        layers = [nn.Linear(input_size, output_size)]
        layers.append(self.activation)

        if batchnormalization:
            layers.append(nn.BatchNorm1d(output_size))

        if dropout:
            layers.append(nn.Dropout(0.1))

        self.linearBlock = nn.Sequential(*layers)

    def forward(self, input):
        """ Output of the model.
        :param torch.Tensor input: input tensor, of shape [batch size x input size]
        :returns: output of the model
        :rtype: torch.Tensor
        """
        output = self.linearBlock(input)
        return output


class identityNN(nn.Module):
    """ Identity module: just give as output the input. Allow backpropagation of gradients. """
    def __init__(self):
        super(identityNN, self).__init__()

    def forward(self, input):
        """ Output of the model.
        :param torch.Tensor input: input tensor, of shape [batch size x input size]
        :returns: output of the model
        :rtype: torch.Tensor
        """
        return input