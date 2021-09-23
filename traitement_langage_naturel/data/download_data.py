#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script pour télécharger les données "Dominick's dataset"

https://www.chicagobooth.edu/research/kilts/datasets/dominicks

Created on Fri Aug 20 14:10:34 2021

@author: jeremylhour
"""
import yaml
import pandas as pd

from unidecode import unidecode

if __name__=='__main__':
    CONFIG_FILE = 'data/dominick_dataset_files.yaml'
    with open(CONFIG_FILE, 'r') as stream:
        config = yaml.safe_load(stream)

    out_file = config.get('out_file')
    base_url = config.get('main_url')
    dico = config.get('categories')

    for item in dico:
        print(f'Currently downloading and parsing data for category : {item}')
        df = pd.read_csv(base_url+dico.get(item))
        examples = '__label__'+ item.replace(' ', '_') + ' ' + df['DESCRIP']

        with open(out_file, 'w') as f:
            for item in examples.to_list():
                f.write("%s\n" % unidecode(item))