FROM sagebionetworks/synapsepythonclient:v4.11.0

RUN apt-get update \
    && apt-get install -y git \
    && rm -rf /var/lib/apt/lists/*
RUN pip install PyYAML
RUN git clone --branch master https://github.com/Sage-Bionetworks/synapseforms.git /root/synapseforms

WORKDIR /root