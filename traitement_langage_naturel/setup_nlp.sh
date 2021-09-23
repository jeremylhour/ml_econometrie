# ------------------------------------------------------------------
# Script de téléchargement des paquets pour traitement du langage naturel
#
# 17 septembre 2021
#
# @author : jeremylhour
# ------------------------------------------------------------------
echo TELECHARGEMENT DES PAQUETS TRAITEMENT LANGAGE NATUREL

pip install unidecode

echo INSTALLATION DE FASTTEXT ET TELECHARGEMENT DES VECTEURS FRANCAIS
git clone https://github.com/facebookresearch/fastText.git
cd fastText
sudo pip install .

echo TELECHARGEMENT DES VECTEURS DE MOT ET REDUCTION DE LA DIMENSION
./download_model.py fr
./reduce_model.py cc.fr.300.bin 100
cd ..

echo TELECHARGEMENT DES DONNEES
python3 data/download_data.py

OUT_FILE=data/dominick_dataset.txt
echo Lignes dans le fichier
wc -l $OUT_FILE

echo Partition aléatoire entre train et test
shuf $OUT_FILE | split -a1 -d -l $(( $(wc -l <$OUT_FILE) * 80 / 100 )) - data/output
mv data/output0 data/train.txt
mv data/output1 data/test.txt