#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
stats_tools:

Created on Sat Apr 22 13:43:34 2023

@author: jeremylhour
"""
import numpy as np
from tqdm import tqdm

from sklearn.base import BaseEstimator
from sklearn.utils.validation import check_X_y, check_array, check_is_fitted
from sklearn.metrics import r2_score

#------------------------------------------------------
# GROUP-LASSO
#------------------------------------------------------
def mse_grad_(w, b, y, X):
    """
    mse_grad_:
        Gradient of Mean Squared Error.
    
    Args:
        w (np.array): Weights.
        b (float): Bias.
        y (np.array): Outcome.
        X (np.array): Design matrix.
    """
    return -2*(y - b - X @ w) @ X / len(X)

def prox_l1(x, alpha):
    """
    prox_l1:
        Proximal operator for the L1-norm.
    
    Args:
        x (np.array): The argument.
        alpha (float): The penalty, as in alpha * norm(x).
    """
    return np.sign(x) * np.maximum(0, np.abs(x) - alpha)

def prox_l2(x, alpha):
    """
    gl_prox_:
        Proximal operator for the L2-norm (not squared).

    Args:
        x (np.array): The argument.
        alpha (float): The penalty, as in alpha * norm(x).
    """
    if np.sqrt(np.sum(x ** 2)) > alpha:
        return x * (1 - alpha / np.sqrt(np.sum(x ** 2)))
    else:
        return x * 0
    
def sparse_group_lasso_prox(x, alpha, gamma, groups):
    """
    sparse_group_lasso_prox:
        Proximal operator for Sparse Group Lasso loss.
    
    Args:
        x (np.array): The argument.
        alpha (float): Overall penalty level.
        gamma (float): Must belong to [0, 1], relative weight of l1-penalty.
        groups (list of lists): The group memberships.
    """
    x = prox_l1(x, gamma * alpha)
    for group in groups:
        x[group] = prox_l2(x[group], (1 - gamma) * alpha)
    return x

class SparseGroupLasso(BaseEstimator):
    """
    SparseGroupLasso:
        Sparse Group Lasso estimator based on the FISTA algorithm.
        The penalty is an convex combination between a Lasso and a Group-Lasso.

        min np.sum((y - b - X @ w) ** 2)
            + gamma * alpha * np.sum(np.abs(w))
            + (1 - gamma) * alpha * np.sum([np.sqrt(np.sum(w[g] ** 2)) for g in groups])

    Args
    ----
    groups : list of lists of integers
        List of feature indices for each group.
    alpha : float, optional (default=1.0)
        Overall penalty strength.
    gamma : float, optional (default=0.0)
        Weight of the Lasso penalty vs. Group-Lasso penalty.  
    max_iter : int, optional (default=1000)
        Maximum number of iterations.
    tol : float, optional (default=1e-4)
        Tolerance for convergence.
    fit_intercept : bool, optional (default=True)
        If True, fits an intercept.
    verbose : boo, optional (default=True)
        If True, displays progress.

    Attributes
    ----------
    coef_ : array, shape (n_features,)
        Estimated coefficients.
    """
    def __init__(self, groups, alpha: float = 1.0, gamma: float = 0.0, max_iter: int = 1000, tol: float = 1e-4, fit_intercept: bool = True, verbose: bool = False):
        self.groups = groups
        self.alpha = alpha
        self.gamma = gamma
        self.max_iter = max_iter
        self.tol = tol
        self.fit_intercept = fit_intercept
        self.verbose = verbose

    def fit(self, X, y):
        X, y = check_X_y(X, y)
        n_, n_features = X.shape
        
        # Initialize constants and variables
        eta = 1 / np.max(2 * np.linalg.eigvals(X.T @ X) / n_)
        t, t_old = 1, 1
        coef, z = np.zeros(n_features), np.zeros(n_features)
        b = 0.

        if self.verbose:
            loop = tqdm(range(self.max_iter))
        else:
            loop = range(self.max_iter)

        for _ in loop:
            # Save parameters
            coef_old = coef.copy() 
            t_old = t
            t = (1 + np.sqrt(1 + 4 * t_old ** 2)) / 2
            delta = (1 - t_old) / t

            # Compute gradient and update coefficients
            grad = mse_grad_(z, b, y, X)
            coef = sparse_group_lasso_prox(z - eta * grad, eta * self.alpha, self.gamma, self.groups)
            if self.fit_intercept:
                b  += eta * 2 * (y - b - X @ coef).mean()

            z = (1 - delta) * coef + delta * coef_old

            # Check convergence
            if np.linalg.norm(coef - coef_old) / np.maximum(1e-9, np.linalg.norm(coef)) < self.tol:
                break
        if (_ == self.max_iter - 1):
            print("Max. number of iterations reached.")
        self.coef_ = coef
        self.intercept_ = b
        return self

    def predict(self, X):
        check_is_fitted(self, 'coef_')
        check_is_fitted(self, 'intercept_')
        X = check_array(X)
        return X @ self.coef_ + self.intercept_
    
    def score(self, X, y):
        y_hat = self.predict(X)
        return r2_score(y, y_hat)
    
#------------------------------------------------------
# SINGULAR VALUE DECOMPOSITION
#------------------------------------------------------
class SVD(BaseEstimator):
    """
    SVD:
        Implements an SVD from numpy as a sklearn transformer.
        Allows to do a Ridge rotation.
    """
    def __init__(self, alpha: float = 0):
        self.alpha = alpha
    
    def fit(self, X):
        self.u, self.w, v = np.linalg.svd(X, full_matrices=False)
        self.v = v.T
        self.rotation = self.v @ np.diag(self.w / (self.w ** 2 + self.alpha))
        return self
    
    def transform(self, X, y=None):
        return X @ self.rotation
    
    def fit_transform(self, X, y=None):
        self.fit(X)
        return self.u
    
if __name__ == '__main__':
    print("Group-Lasso test")
    n_ = 100_000
    beta = np.concatenate((np.ones(3), -2*np.ones(1), 0*np.ones(1), np.zeros(10)))
    X = np.random.normal(size=(n_, len(beta)))
    y = 12 + X @ beta + np.random.normal(size=n_)

    print(beta)

    estimator = MixedGroupLasso(
        groups = [range(3), range(3, 5), range(5, 15)],
        gamma=.2,
        verbose=True
        )
    estimator.fit(X, y)
    print('w:', estimator.coef_)
    print('b:', estimator.intercept_)
    y_hat = estimator.predict(X)