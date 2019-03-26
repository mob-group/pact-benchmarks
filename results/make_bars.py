#!/usr/bin/env python

import matplotlib.pyplot as plt

from collections import namedtuple

# Schema for each bar chart:
# - Benchmark (e.g. abinit)
# - Dataset (e.g. Ti22)
# - Our speedup
# - [opt] Their speedup
# - Our impl
# - [opt] Their impl
BarData = namedtuple('BarData', [
    'benchmark', 'dataset',
    'our_speed', 'our_impl',
    'exp_speed', 'exp_impl'
])
BarData.__new__.__defaults__ = (None,) * 2

data = [
    [
        BarData('Abinit', 'Ti22', 1.2, 'MKL')
    ]
]

# Take an axis and some bar data, then plot data on the axis
def bar(ax, data):
    print(data)

if __name__ == "__main__":
    fig, axes = plt.subplots(2, 5, sharey=True, figsize=(7,3))

    for ax_row, data_row in zip(axes, data):
        for pair in zip(ax_row, data_row):
            bar(*pair)

    fig.tight_layout()
    plt.savefig('out.pdf')
