
# Data input file

This application accepts **CSV** and **TXT** format data files.  When converting files from EXCEL,make sure that numbers are converted to general format.  **No thousand comma "," s.**   
Use **blank** for missing passage 

# Data Input Format

Data  consists of Date, Years in number format from the earliest  to the latest.   

**Example** 

| Date | |  1980| | 1981    | | 1983  | | 1984  | | ....  | | 2019  | 
|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|
|20-Jun  | |5   | |   | | 0 | | 1 | | .... | |  0  |
|21-Jun  | |15   | |10  | | 2 | |  | | .... | |  0  |
|22-Jun  | |20   | |20  | | 4 | | 6 | |.... | |  2  |
|....    | |....   | |.... | | .... | | .... | | .... | |  ....  |
|18-Sep  | |2   | |3  | |  | |	 | | .... | |	 6  |
|19-Sep  | |1   | | | | 5 | |	5 | | .... | |  2  |
|20-Sep  | |0    | |0  | | 3 | |	1 | | .... | |    |
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|
 
---
**Note**  
The model performs better when peak timing is situated around 20-30 days from the first day of the passage counting. Shift dates accordingly. 


