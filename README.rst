Proposition
===========

Let

.. math::

  A = \left(\begin{pmatrix}
    0 \\ 1 \\ 2 \\ 3 \\ 4 \\ 5 \\ 6
  \end{pmatrix}, \begin{pmatrix}
    1 \\ 2 \\ 3 \\ 4 \\ 5 \\ 6 \\ 0
  \end{pmatrix}, \begin{pmatrix}
    2 \\ 3 \\ 4 \\ 5 \\ 6 \\ 0 \\ 1
  \end{pmatrix}, \begin{pmatrix}
    3 \\ 4 \\ 5 \\ 6 \\ 0 \\ 1 \\ 2
  \end{pmatrix}, \begin{pmatrix}
    4 \\ 5 \\ 6 \\ 0 \\ 1 \\ 2 \\ 3
  \end{pmatrix}, \begin{pmatrix}
    5 \\ 6 \\ 0 \\ 1 \\ 2 \\ 3 \\ 4
  \end{pmatrix}, \begin{pmatrix}
    6 \\ 0 \\ 1 \\ 2 \\ 3 \\ 4 \\ 5
  \end{pmatrix}\right)

:math:`A` is a basis of :math:`\mathbb F_7^7`.

The Problem
===========

It does not exist free software which allows one to answer this question.

The Solution
============

.. math::

  L(A, 0) = Span\left(\begin{pmatrix}
    1 \\ 5 \\ 1 \\ 0 \\ 0 \\ 0 \\ 0
  \end{pmatrix}, \begin{pmatrix}
    2 \\ 4 \\ 0 \\ 1 \\ 0 \\ 0 \\ 0
  \end{pmatrix}, \begin{pmatrix}
    3 \\ 3 \\ 0 \\ 0 \\ 1 \\ 0 \\ 0
  \end{pmatrix}, \begin{pmatrix}
    4 \\ 2 \\ 0 \\ 0 \\ 0 \\ 1 \\ 0
  \end{pmatrix}, \begin{pmatrix}
    5 \\ 1 \\ 0 \\ 0 \\ 0 \\ 0 \\ 1
  \end{pmatrix}\right)

In other words: :math:`A` is not a basis of :math:`\mathbb F_7^7`.

