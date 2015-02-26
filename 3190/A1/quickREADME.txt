Program Usage
To run the program type:

gfortran rpn.f95
./a.out < polish.text

You may redirect the input stream from a text file, it will open the file and read to the end,
spitting out the output. 

You may also run the program simply by typing 

./a.out

At this point the stdin will be open and you may input a regular mathematical equation you
want into reverse polish. Type it in and press enter. 
To quit, press q (LOWER CASE) and press enter. 
DO NOT put q as the first character in any string or the
program will terminate

There is no prompt because I assumed we would just be running these through with text files
for quick marking