#!/usr/bin/env python

import matplotlib.pyplot as plt
import seaborn as sns

from collections import namedtuple

from sizes import *

sns.set()
sns.set_context("paper")

BarData = namedtuple('BarData', [
    'benchmark', 'dataset',
    'a_speed', 'a_impl',
    'b_speed', 'b_impl'
])
BarData.__new__.__defaults__ = (None,) * 2

data = [
    [
        BarData('', 'ResNet-152', 2.238527805, 'OpenMP', 4.498543381, 'OpenCL'),
        BarData('', 'VGG-16', 2.786963245, 'OpenMP', 8.820989634, 'OpenCL'),
        BarData('', 'DenseNet-201', 1.931831601, 'OpenMP', 2.255963752, 'OpenCL'),
    ]
]

def col(i):
    return sns.color_palette()[i]

def bar(ax, data):
    bar_params = { 'width' : 0.8 }
    ret = []

    ret.append(ax.bar(-1, 1, color=col(0), **bar_params))
    ret.append(ax.bar(0, data.a_speed, color=col(3), **bar_params))
    ret.append(ax.bar(1, data.b_speed, color=col(5), **bar_params))

    ax.set_xlim(-1.5, 1.5)

    # y_max = max(data.a_speed, data.b_speed) * 1.1
    # ax.set_ylim(0, y_max)

    ax.axhline(y=1, color='black', lw=0.8)

    ax.set_xticks([-1, 0, 1])
    ax.set_xticklabels([])
    ax.tick_params(axis='both', labelsize=6)

    ax.set_title('{} {}'.format(data.benchmark, data.dataset), fontsize=8)
    return ret

if __name__ == "__main__":
    fig, axes = plt.subplots(1, 3, sharey=False, figsize=(col_w(1),col_w(0.35)), squeeze=False)

    for ax_row, data_row in zip(axes, data):
        for pair in zip(ax_row, data_row):
            legs = bar(*pair)

    fig.legend(legs, ['Baseline', 'OpenMP', 'OpenCL'],
            bbox_to_anchor=(0.5, -0.02), loc='lower center', ncol=3, fontsize=8)

    fig.text(0.025, 0.5, "Speedup ($\\times$)", rotation=90, va='center',
            fontsize=8)

    fig.tight_layout()
    fig.subplots_adjust(bottom=0.25, left=0.15)

    sns.despine(fig)
    plt.savefig('nnperf.pdf')
