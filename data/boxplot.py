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

def best_by_col(data, col):
	best = data[0]
	best_val = best[len(best) - 1][col]
	for d in data:
		cur_val = d[len(d) - 1][col]
		if cur_val < best_val:
			best = d
			best_val = cur_val
	return best

def mean_by_col(datas, col):
	gen_val = [extract_col(d, col) for d in datas]
	return [np.mean(x) for x in np.transpose(gen_val)]

def extract_col(data, col):
	return [data[g][col] for g in range(len(data))]

def ga_ccga_compare(title, ga_data, ccga_data, col):
	def mean_best(data, col):
		m = mean_by_col(data, col)
		b = extract_col(best_by_col(data, col), col)
		return (m, b)

	ga_mean, ga_best = mean_best(ga_data, col)
	ccga_mean, ccga_best = mean_best(ccga_data, col)

	lines = plot_data(ga_mean, ga_best)
	labels = ['GA mean', 'GA best']

	lines.append(plot_data(ccga_mean, ccga_best))
	labels.extend('CCGA mean', 'CCGA best')

	if col == BDF_COL:
		lines.append(plt.plot([0, 100], [1.0, 1.0], 'k--'))
		labels.append('Schedulable')
		plt.ylabel('Breakdown Frequency')
	else:
		plt.ylabel('Schedulability (%)')
	plt.xlabel('Generation')

	plt.legend([l[0] for l in lines], labels)
	plt.title(title)
	plt.show()

if __name__ == '__main__':
	ga3 = read_files('ga_ava_3x3_', 20)
	ccga3 = read_files('ccga_ava_3x3_', 20)
	ga_ccga_compare('AVA 3x3 Breakdown Frequency', ga3, ccga3, BDF_COL)
	ga_ccga_compare('AVA 3x3 Schedulability', ga3, ccga3, SHCED_COL)


