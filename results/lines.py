#!/usr/bin/env python

import matplotlib.pyplot as plt
import seaborn as sns

from collections import namedtuple

from sizes import *

sns.set()
sns.set_context("paper")

xs = [1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
mkls = [0.9090909091,
0.9361702128,
1.168674699,
1.218390805,
1.318644068,
1.429545455,
1.445880452,
1.474940334,
1.514814815,
1.512070227,
1.517549078,
1.526496565]

cudas = [
0.6666666667,
0.88,
1.197530864,
1.239766082,
1.40433213,
1.564676617,
1.621376812,
1.70718232,
1.791894852,
1.817941953,
1.837896254,
1.868468468]

if __name__ == "__main__":
    fig, ax = plt.subplots(1, 1, sharey=False, figsize=(col_w(1),col_w(0.5)),
            squeeze=True)
    ax.plot(xs, mkls)
    ax.plot(xs, cudas)

    ax.legend(['MKL', 'CUDA'])
    ax.set_ylabel('Speedup ($\\times$)', fontsize=8)
    ax.set_xlabel('Problem Size (atoms)', fontsize=8)
    ax.tick_params(axis='both', labelsize=6)

    fig.tight_layout()

    sns.despine(fig)
    plt.savefig('lines.pdf')
