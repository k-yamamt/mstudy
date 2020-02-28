import os

plotfiles = []

for filename in os.listdir('./aws.amazon.com/'):
    if filename[-4:] == '.pdf':
        plotfiles.append(filename)

with open('addtex.txt','w') as f:
    id = 0
    day = 0
    for filename in plotfiles:
        if (id == 0):
            f.write('\\begin{figure}[tb]\n')
            f.write('\\begin{center}\n')
            f.write('\\subfigure[3:00 - 4:00]{\n')
            f.write('\\includegraphics[width=.33\\columnwidth]{0}\n'.format('{'+filename+'}'))
            f.write('}~\n')
            id = 1
        elif(id == 1):
            f.write('\\subfigure[7:00 - 8:00]{\n')
            f.write('\\includegraphics[width=.33\\columnwidth]{0}\n'.format('{'+filename+'}'))
            f.write('}~\n')
            id = 2
        elif(id == 2):
            f.write('\\subfigure[12:00 - 13:00]{\n')
            f.write('\\includegraphics[width=.33\\columnwidth]{0}\n'.format('{'+filename+'}'))
            f.write('}\\\\\n')
            id = 3
        elif(id == 3):
            f.write('\\subfigure[17:00 - 18:00]{\n')
            f.write('\\includegraphics[width=.33\\columnwidth]{0}\n'.format('{'+filename+'}'))
            f.write('}~\n')
            id = 4
        elif(id == 4):
            f.write('\\subfigure[20:00 - 21:00]{\n')
            f.write('\\includegraphics[width=.33\\columnwidth]{0}\n'.format('{'+filename+'}'))
            f.write('}\n')
            f.write('\\caption{'+'{0}gatsu{1}nichi({2}) aws.amazon.com wotaisyo'.format(int(filename[-20:-18]),int(filename[-18:-16]),'dayid='+str(day%7))+'}\n')
            id = 0
            day += 1




            

