This is the standard bundle for a National Centers for Environmental
Information archival information package (also referred to as an AIP
or an accession). Each AIP maintained by NCEI using this AIP bundle
is given a unique integer identifier known as an 'accession id'. All
information related to that accession identifier is contained within a
directory using the accession id as the directory name. The accession
directory has the following standard structure:

<accession id>: This directory

NCEI-Readme.txt: This file.

about: Directory. Contains AIP related metadata including but
not limited to the following two standard files.

journal.txt: Text file. Contains any notes, actions taken by NCEI, etc.,
relating to this accession.

<accession id>.md5: Text file: contains MD5 checksums for all files
in this accession except for the <accession id>.md5 file itself.

Other metadata files may include correspondence between NCEI and
a data provider, files created by NCEI to document contents of the AIP, etc.

data: Directory. All archived data are located in the 'data' directory.

0-data: Directory. Contains the originator's data unmodified from its initial digital
format as submitted to NCEI. The initial source for the archived data should be
documented in the header of the <accession id>/about/journal.txt file after
the keyword, 'Source'.

1-data: Optional directory. May contain processed version(s) of originator's
data from '0-data' directory. The filename convention should indicate the
relationship between the original file in '0-data' and the processed file
in '1-data'. Any processing done by NCEI to create files in '1-data- should be
annotated in <accession id>/about/journal.txt to explainhow files in
1-data were derived from the files in 0-data.

For further information about this accession see:

./about/journal.txt
