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