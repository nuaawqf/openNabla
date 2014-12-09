
g95 -O2 -c -ftrace=full  ..\..\kernel\precision.f90
g95 -O2 -c -ftrace=full  ..\..\kernel\scheme.f90
g95 -O2 -c -ftrace=full  ..\..\kernel\listlib.f90
g95 -O2 -c -ftrace=full  ..\..\kernel\strlib.f90

g95 -O2 -c -ftrace=full  ..\..\kernel\fileio.f90
g95 -O2 -c -ftrace=full  ..\..\kernel\info.f90
g95 -O2 -c -ftrace=full  ..\..\kernel\iolib.f90

g95 -O2 -c -ftrace=full  ..\..\kernel\vec3dlib.f90

g95 -O2 -c -ftrace=full  ..\..\kernel\mshbase.f90
g95 -O2 -c -ftrace=full  ..\..\kernel\mshfluent.f90
g95 -O2 -c -ftrace=full  ..\..\kernel\mshfoam.f90
g95 -O2 -c -ftrace=full  ..\..\kernel\mshtool.f90



g95 -O2 -o main.exe main.f90 *.o
del *.o *.mod
