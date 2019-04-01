#!/usr/bin/env python

import matplotlib.pyplot as plt
import seaborn as sns

from collections import namedtuple

from sizes import *

sns.set()
sns.set_context("paper")
sns.set_palette("GnBu_d")

# BarData = namedtuple('BarData', [
#     'benchmark', 'dataset',
#     'a_speed', 'a_impl',
#     'b_speed', 'b_impl'
# ])
# BarData.__new__.__defaults__ = (None,) * 2

# data = [
#     [
#         BarData('', 'ResNet-152', 9.362466855, 'MKL', 5.978566634, 'CUDA'),
#         BarData('', 'VGG-16', 10.85887595, 'MKL', 7.824449943, 'CUDA'),
#         BarData('', 'DenseNet-201', 5.609201284, 'MKL', 3.307105118, 'CUDA'),
#         BarData('Pathsample', 'PFold', 2.652436609, 'MKL', 2.092599828, 'CUDA'),
#         BarData('Pathsample', 'NGT', 1.168143249, 'MKL', 1.010142973, 'CUDA'),
#     ],
#     [
#         BarData('Abinit', 'Titanium', 1.542322835, 'MKL', 1.919559004, 'CUDA'),
#         BarData('Abinit', 'Water', 1.200775946, 'MKL', 1.281573499, 'CUDA'),
#         BarData('NWChem', 'Buckyball', 1.218503937, 'MKL', 0.6627408994, 'CUDA'),
#         BarData('NWChem', 'Pentacene', 1.207220574, 'MKL', 0.4516188714, 'CUDA'),
#         BarData('Parboil', 'SGEMM', 14.686275195560686, 'MKL', 19.279746767639924, 'CUDA'),
#     ]
# ]
BarData = namedtuple('BarData', [
    'benchmark', 'speedup',
])

data = [
    BarData('Parboil', 19.27974677),
    BarData('Darknet', 8.292625701),
    BarData('Pathsample', 1.76023462),
    BarData('ABINIT', 1.56845655),
    BarData('NWChem', 1.212849134),
]

upper_limit = 5

def bar(ax, i, b):
    bar_params = { 'width' : 0.8 }

    if b.speedup > upper_limit:
        ax.set_ylim(0, upper_limit)
        ax.bar(i, upper_limit, **bar_params)
        ax.text(i, upper_limit * 1.05, '{:.1f}$\\times$'.format(b.speedup), fontsize=8, ha='center')
        ax.text(i, upper_limit * 0.95, b.benchmark, rotation=90, fontsize=8,
                color='white', ha='center', va='top')
    else:
        ax.bar(i, b.speedup, **bar_params)
        ax.text(i, b.speedup * 1.05, b.benchmark, rotation=90, fontsize=8,
                ha='center', va='bottom')

    # ret = []

    # ret.append(ax.bar(-1, 1, **bar_params))
    # ret.append(ax.bar(0, data.a_speed, **bar_params))
    # ret.append(ax.bar(1, data.b_speed, **bar_params))

    # ax.set_xlim(-1.5, 1.5)

    # y_max = max(data.a_speed, data.b_speed) * 1.1
    # ax.set_ylim(0, y_max)

    ax.axhline(y=1, color='black', lw=0.8)

    ax.set_xticks([])
    ax.tick_params(axis='both', labelsize=6)

    # ax.set_title('{} {}'.format(data.benchmark, data.dataset), fontsize=8)
    # return ret
    pass

if __name__ == "__main__":
    fig, ax = plt.subplots(1, 1, sharey=False,
            figsize=(col_w(1),col_w(0.6)))

    for i, b in enumerate(data):
        bar(ax, i, b)

    ax.set_ylabel('Speedup ($\\times$)', fontsize=8)

    fig.tight_layout()
    sns.despine(fig)
    plt.savefig('headline.pdf')
