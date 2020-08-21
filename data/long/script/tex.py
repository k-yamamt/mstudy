import os
x = list()
x.append('\\subfigure[$')
x.append(' \\sim ')
x.append('$Min]{\n')
x.append('\\includegraphics[width=0.33\\hsize]{')
x.append('.pdf}\n')
x.append('}~')
x.append('}\\\\\n')
for t in range(0,60,3):
    print (x[0] + str(t) + x[1] + str(t+1) + x[2] + x[3] + str(t) + '-' + str(t+1) + x[4] + x[5])
    print (x[0] + str(t+1) + x[1] + str(t+2) + x[2] + x[3] + str(t+1) + '-' + str(t+2) + x[4] + x[5])
    print (x[0] + str(t+2) + x[1] + str(t+3) + x[2] + x[3] + str(t+2) + '-' + str(t+3) + x[4] + x[6])