#!/usr/bin/env python

import matplotlib.pyplot as plt
import seaborn as sns

from collections import namedtuple

sns.set()
sns.set_context("paper")

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
        BarData('Abinit', 'Ti22', 1.2, 'MKL', 1.4, 'MKL')
    ],
    [
        BarData('', 'VGG-16', 2.2, 'CUDA', 2.8, 'CUDA')
    ]
]

def has_expert(data):
    return None not in [data.exp_speed, data.exp_impl]

def best_speed(row):
    return max([max(d.exp_speed, d.our_speed) for d in row])

def expert_label(data):
    if data.exp_impl is not None:
        return 'Expert'
    else:
        return ''

# Take an axis and some bar data, then plot data on the axis
def bar(ax, data, y_max):
    bar_params = { 'width' : 0.8 }

    ax.bar(-1, 1, **bar_params)
    ax.bar(0, data.our_speed, **bar_params)
    if has_expert(data):
        ax.bar(1, data.exp_speed, **bar_params)

    ax.set_xlim(-1.5, 1.5)
    ax.set_ylim(0, y_max * 1.1)

    ax.axhline(y=1, color='black', lw=0.8)

    ax.set_xticks([-1, 0, 1])
    ax.set_xticklabels(['Base', '{}'.format(data.our_impl), expert_label(data)])
    ax.tick_params(axis='x', labelsize=8)

    ax.set_title('{} {}'.format(data.benchmark, data.dataset))

if __name__ == "__main__":
    fig, axes = plt.subplots(2, 5, sharey='row', figsize=(7,3))

    for ax_row, data_row in zip(axes, data):
        for pair in zip(ax_row, data_row):
            bar(*pair, best_speed(data_row))

    fig.tight_layout()
    sns.despine(fig)
    plt.savefig('perf.pdf')
