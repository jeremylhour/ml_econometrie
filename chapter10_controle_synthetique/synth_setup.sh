# ------------------------------------------------------------------
# Script de téléchargement des données pour l'exemple
# sur  le contrôle synthétique
# Abadie et al. (2010)
#
# 15 juillet 2021
#
# @author : jeremylhour
# ------------------------------------------------------------------
sudo apt-get install unzip

echo TELECHARGEMENT DES DONNEES POUR LE CONTROLE SYNTHETIQUE

URL=https://web.stanford.edu/~jhain/Synth_Matlab/Synth_MATLAB.zip
FOLDER=Synth_MATLAB
FILE=MLAB_data.txt

mkdir ../data/

curl $URL -o $FOLDER.zip
unzip $FOLDER.zip -d $FOLDER
mv $FOLDER/$FILE ../data/$FILE
rm -r $FOLDER
rm $FOLDER.zip