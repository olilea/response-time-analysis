import matplotlib.pyplot as plt
import numpy as np

GENERATIONS = 101
GEN_COL = 0
BDF_COL = 1
SCHED_COL = 2

def plot_data(*data):
	ls = []
	for d in data:
		ls.append(plt.plot(range(GENERATIONS), np.array(d)))
	return ls

def read_files(prefix, count):
	ds = []
	for i in range(1, count + 1):
		ds.append(np.genfromtxt(prefix + str(i) + '.csv',
			delimiter=',',
			skip_header=1))
	return ds

def best_by_col(data, col, low_best=True):
	best = data[0]
	best_val = best[len(best) - 1][col]
	for d in data:
		cur_val = d[len(d) - 1][col]
		if low_best:
			if cur_val < best_val:
				best = d
				best_val = cur_val
		else:
			if cur_val > best_val:
				best = d
				best_val = cur_val
	return best

def mean_by_col(datas, col):
	gen_val = [extract_col(d, col) for d in datas]
	return [np.mean(x) for x in np.transpose(gen_val)]

def extract_col(data, col):
	return [data[g][col] for g in range(len(data))]

def mean_best(data, col, low_best=True):
	m = mean_by_col(data, col)
	b = extract_col(best_by_col(data, col, low_best), col)
	return (m, b)

def compare(title, data1, data2, labels, col, max_y=None, low_best=True):
	d1_mean, d1_best = mean_best(data1, col, low_best)
	d2_mean, d2_best = mean_best(data2, col, low_best)

	lines = plot_data(d1_mean, d1_best)
	lines.extend(plot_data(d2_mean, d2_best))

	if col == BDF_COL:
		lines.append(plt.plot([0, 100], [1.0, 1.0], 'k--'))
		plt.ylabel('Breakdown Frequency')
	else:
		plt.ylabel('Schedulability (%)')
	plt.xlabel('Generation')

	plt.legend([l[0] for l in lines], labels)
	plt.title(title)

	if max_y is not None:
		x1,x2,y1,y2 = plt.axis()
		plt.axis((x1,x2,y1, max_y))

	plt.show()

if __name__ == '__main__':
	ga3 = read_files('ga_ava_3x3_', 20)
	ccga3 = read_files('ccga_ava_3x3_', 20)

	hopri_3_ava = read_files('ccga_ava_3x3_PRI_', 20)

	labels = ['GA mean', 'GA best', 'CCGA mean', 'CCGA best', 'Schedulable']
	compare('AVA 3x3 Breakdown Frequency', ga3, ccga3, labels, BDF_COL, max_y=2.0)
	compare('AVA 3x3 Schedulability', ga3, ccga3, labels, SCHED_COL, low_best=False)


	labels = ['HO-PRI mean', 'HO-PRI best', 'CCGA mean', 'CCGA best', 'Schedulable']
	compare('HO-PRI 3x3 Breakdown Frequency', hopri_3_ava, ccga3, labels, BDF_COL, max_y=2.0)

