# name of datasets to process and downloaded to the 'downloaded-datasets' dir
datasets = ['CRISPR', 'Cyanobacteria']

from itertools import product

################################################################################
# Download and parse IPC scheme

rule download_ipc:
    output:
        'data/ipc-scheme.xml'
    shell:
        """
        URL=https://www.wipo.int/ipc/itos4ipc/ITSupport_and_download_area/20230101/MasterFiles/ipc_scheme_20230101.zip
        cd data
        wget $URL
        unzip ipc_scheme_20230101.zip -x FR_ipc_scheme_20230101.xml
        mv EN_ipc_scheme_20230101.xml ipc-scheme.xml
        rm ipc_scheme_20230101.zip
        """
        
rule parse_ipc:
    input:
        script = 'scripts/parse-ipc.R',
        xs = [
            'data/ipc-scheme.xml'
        ]
    output:
        'data/ipc.tsv.gz'
    conda: 'env-r.yml'
    shell:
        """
        ./{input.script}
        """
        
################################################################################
# Pre-process lens.org downloads

rule parse_dataset:
    input:
        script = 'scripts/parse-datasets.R',
        xs = 'downloaded-datasets/{dataset}'
    output:
        'data/{dataset}/patents.tsv.gz',
        'data/{dataset}/ipc.tsv.gz',
        'data/{dataset}/works.tsv.gz',
        'data/{dataset}/links-patent-work.tsv.gz'
    conda: 'env-r.yml'
    threads: 4
    shell:
        """
        ./{input.script} {threads} {input.xs}
        """
        
def dataset_data(wildcards):
    "Helper to list all output files from pre-processing"
    xs = [
        'patents.tsv.gz',
        'ipc.tsv.gz',
        'works.tsv.gz',
        'links-patent-work.tsv.gz'
    ]
    for data, file in product(datasets, xs):
      yield 'data/{}/{}'.format(data, file)
        
################################################################################
# Create overview for datasets

rule overview:
    input:
        script = 'scripts/overview.R',
        helper = 'scripts/helper_load.R',
        xs = dataset_data
    output:
        'analysis/overview-datasets.png',
        'analysis/overview-journals.png',
    shell:
        """
        ./{input.script}
        """
        
################################################################################

rule ipc:
    input:
        script = 'scripts/ipc-analysis.R',
        xs = [
            'data/data-patents.tsv.gz',
            'data/data-works.tsv.gz',
            'data/data-link.tsv.gz',
            'data/data-patents-ipc.tsv.gz',
            'data/ipc.tsv.gz'
        ]
    output:
        'data/ipc-count-correlations.png',
        'data/ipc-mean-var.png',
        'data/ipc-year-correlation.png',
        'data/ipc-mc.png',
        'data/ipc-heatmap.png',
        'data/ipc-markov-chain.tsv.gz'
    shell:
        """
        ./{input.script}
        """


################################################################################

rule all:
    input:
        'data/ipc-scheme.xml',
        'data/ipc.tsv.gz',
        [ 'data/{}/patents.tsv.gz'.format(i) for i in datasets ],
        'analysis/overview-datasets.png',
        'analysis/overview-journals.png',
        #'data/ipc-heatmap.png',
