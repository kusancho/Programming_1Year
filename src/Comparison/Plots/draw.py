import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import pandas as pd

plotName = "SeqVsParallel0,8"
labels = []
def add_label(violin, label):
    color = violin["bodies"][0].get_facecolor().flatten()
    labels.append((mpatches.Patch(color=color), label))

def draw(file, name, axs):
	df = pd.read_csv(file)
	data = [[d[1][0], d[1][1:]] for d in df.iterrows()]

	plt.ioff()

	r = [d[1] / 10 for d in data]
	lbl = [d[0] / 1000 for d in data]
	
	add_label(axs.violinplot(r,
	   positions=lbl,
	   widths = 2,
	   showmeans=False,
	   showmedians=True),
 	   name)


def drawFiles (filesWithLegend, out):	
	fig = plt.figure(plotName)
	axs = plt.axes()

	axs.yaxis.grid(True)
	axs.set_xlabel('Длина стороны матрицы * 1000')
	axs.set_ylabel('Время умножения (миллисекунды) * 10')

	for (file,legend) in filesWithLegend:
		draw(file, legend, axs)

	plt.legend(*zip(*labels), loc=2)

	plt.savefig(out)
	plt.close(fig)

drawFiles([('parallel0,8.csv', "parallel"), ('seq0,8.csv', "seq")], 
	plotName + ".pdf")
	
