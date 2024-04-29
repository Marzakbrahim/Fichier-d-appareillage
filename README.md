# Fichier-d-appareillage
## Description
This project aims to compare two input files containing contract and identity data and produce three output files based on their relationships.
## Input Files
There are two input files:
1- Contract File (Fichier n°1):
- Strecture :
  01 E-Fic1.
    05 E-Fic1-RefCtr PIC X(09).
    05 FILLER PIC X.
    05 E-Fic1-Sit-Ctr PIC X.
2- Identity File (Fichier n°2):
- Strecture :
  01 E-Fic2.
    05 E-Fic2-RefCtr PIC X(09).
    05 FILLER PIC X.
    05 E-Fic2-CodItd PIC 9(07).
    05 FILLER PIC X.
    05 E-Fic1-Sit-Idt PIC X.
## Output Files
Three output files are generated:
1- File 1 (1PAS2): Records from File 1 absent in File 2.
2- File 2 (2PAS1): Records from File 2 absent in File 1.
3- File 3 (SOR): Records present in both files.
## Logic and Considerations
- Records are sorted in ascending order.
- Each contract corresponds to an identity, but there can be multiple identities for a contract.
- The program is structured in COBOL and follows a procedural approach.
- Error handling includes identifying and logging errors related to empty references or incorrectly defined statuses.
## Usage
1-Compile and execute the COBOL program Appariement.cbl.
2- Ensure the input files (FIC1.txt and FIC2.txt) are correctly formatted and located at the specified paths.
3- Output files (Fic1non2.txt, Fic2non1.txt, Fic1et2.txt, Fic-Err.txt) will be generated in the specified directory.
## Error Handling
- Errors are categorized into three types:
  1- Reference of the contract is empty.
  2- Contract status is incorrectly defined (should be 'C', 'R', or 'S').
  3- Identity status is incorrectly defined (should be 'C', 'R', or 'S').
- Error messages are written to the error log file (Fic-Err.txt).
## Conclusion
The program provides a solution for comparing contract and identity data files, generating outputs based on their relationships, and handling errors effectively.  
