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
    'a_speed', 'a_impl',
    'b_speed', 'b_impl'
])
BarData.__new__.__defaults__ = (None,) * 2

data = [
    [
        BarData('', 'ResNet-152', 9.362466855, 'MKL', 5.978566634, 'CUDA'),
        BarData('', 'VGG-16', 10.85887595, 'MKL', 7.824449943, 'CUDA'),
        BarData('', 'DenseNet-201', 5.609201284, 'MKL', 3.307105118, 'CUDA'),
        BarData('Pathsample', 'PFold', 2.652436609, 'MKL', 2.092599828, 'CUDA'),
        BarData('Pathsample', 'NGT', 1.168143249, 'MKL', 1.010142973, 'CUDA'),
    ],
    [
        BarData('Abinit', 'Titanium', 1.542322835, 'MKL', 1.919559004, 'CUDA'),
        BarData('Abinit', 'Water', 1.200775946, 'MKL', 1.281573499, 'CUDA'),
        BarData('NWChem', 'Buckyball', 1.218503937, 'MKL', 0.6627408994, 'CUDA'),
        BarData('NWChem', 'Pentacene', 1.207220574, 'MKL', 0.4516188714, 'CUDA'),
        BarData('Parboil', 'SGEMM', 14.686275195560686, 'MKL', 19.279746767639924, 'CUDA'),
    ]
]

def has_expert(data):
    return None not in [data.exp_speed, data.exp_impl]

def best_speed(row):
    return max([max(d.exp_speed if d.exp_speed is not None else 0, d.our_speed) for d in row])

def expert_label(data):
    if data.exp_impl is not None:
        return 'Expert'
    else:
        return ''

# Take an axis and some bar data, then plot data on the axis
def bar(ax, data):
    bar_params = { 'width' : 0.8 }
    ret = []

    ret.append(ax.bar(-1, 1, **bar_params))
    ret.append(ax.bar(0, data.a_speed, **bar_params))
    ret.append(ax.bar(1, data.b_speed, **bar_params))
    # if has_expert(data):
    #     ret.append(ax.bar(1, data.exp_speed, **bar_params))
    #     if data.exp_speed > (data.our_speed * 2.5):
    #         ax.text(1, data.our_speed * 2.5, '{:.0f}x  '.format(data.exp_speed),
    #                 rotation=90, fontsize=6, va='top', ha='center',
    #                 color='white')

    ax.set_xlim(-1.5, 1.5)

    y_max = max(data.a_speed, data.b_speed) * 1.1
    ax.set_ylim(0, y_max)

    ax.axhline(y=1, color='black', lw=0.8)

    ax.set_xticks([-1, 0, 1])
    # # ax.set_xticklabels(['Base', '{}'.format(data.our_impl), expert_label(data)])
    ax.set_xticklabels([])
    ax.tick_params(axis='x', labelsize=8)

    ax.set_title('{} {}'.format(data.benchmark, data.dataset))
    return ret

if __name__ == "__main__":
    fig, axes = plt.subplots(2, 5, sharey=False, figsize=(7,3))

    for ax_row, data_row in zip(axes, data):
        for pair in zip(ax_row, data_row):
            legs = bar(*pair)

    fig.legend(legs, ['Baseline', 'MKL', 'CUDA'],
            bbox_to_anchor=(0.5, 0.02), loc='lower center', ncol=3)
    fig.tight_layout()
    fig.subplots_adjust(bottom=0.15)

    sns.despine(fig)
    plt.savefig('perf.pdf')
