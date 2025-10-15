FROM rocker/tidyverse:3.6.3-ubuntu18.04

RUN apt-get update && apt-get install python3-pip -y
RUN pip3 install synapseclient==2.5.1
RUN Rscript -e 'install.packages("optparse")'
RUN Rscript -e 'devtools::install_github("Sage-Bionetworks/synapseforms", ref="master")'
RUN git clone --branch master https://github.com/Sage-Bionetworks/synapseforms.git /root/synapseforms
