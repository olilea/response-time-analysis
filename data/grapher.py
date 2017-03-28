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

def compare(title, data1, data2, labels, col, max_x=None, min_y=None, max_y=None, low_best=True):
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


	x1,x2,y1,y2 = plt.axis()
	if max_x is not None:
		x2 = max_x
	if max_y is not None:
		y2 = max_y
	if min_y is not None:
		y1 = min_y
	plt.axis((x1,x2,y1, max_y))

	plt.show()

if __name__ == '__main__':
	from os import path
	ho_ga_ava = read_files(path.join('ga_ava_3x3', 'ga_ava_3x3_'), 10)
	ho_ccga_ava = read_files(path.join('ccga_ava_3x3', 'ccga_ava_3x3_'), 10)

	ho_ga_25 = read_files(path.join('ga_gen_m25_u6_3x3', 'ga_gen_m25_u6_3x3_'), 10)
	ho_ccga_25 = read_files(path.join('ccga_gen_m25_u6_3x3', 'ccga_gen_m25_u6_3x3_'), 10)

	ho_ga_5 = read_files(path.join('ga_gen_m5_u6_3x3', 'ga_gen_m5_u6_3x3_'), 10)
	ho_ccga_5 = read_files(path.join('ccga_gen_m5_u6_3x3', 'ccga_gen_m5_u6_3x3_'), 10)

	hopri_3_ava = read_files('ccga_ava_3x3_PRI/ccga_ava_3x3_PRI_', 10)

	labels = ['HO-GA mean', 'HO-GA best', 'HO-CCGA mean', 'HO-CCGA best', 'Schedulable']
	compare('AVA Breakdown Frequency', ho_ga_ava, ho_ccga_ava, labels, BDF_COL, min_y=0.5, max_y=2.0)
	# compare('AVA Schedulability', ho_ga_ava, ho_ccga_ava, labels, SCHED_COL, low_best=False)

	compare('Generated - 0.25 Breakdown Frequency', ho_ga_25, ho_ccga_25, labels, BDF_COL, max_y=2.0)
	# compare('Generated - 0.25 Schedulability', ho_ga_25, ho_ccga_25, labels, SCHED_COL, low_best=False)

	compare('Generated - 0.5 Breakdown Frequency', ho_ga_5, ho_ccga_5, labels, BDF_COL, max_y=2.0)
	# compare('Generated - 0.5 Schedulability', ho_ga_5, ho_ccga_5, labels, SCHED_COL, low_best=False)

	ho_pri_ava = read_files(path.join('ccga_ava_3x3_PRI', 'ccga_ava_3x3_PRI_'), 10)
	ho_pri_25 = read_files(path.join('ccga_gen_m25_u6_3x3_PRI', 'ccga_gen_m25_u6_3x3_PRI_'), 10)
	ho_pri_5 = read_files(path.join('ccga_gen_m5_u6_3x3_PRI', 'ccga_gen_m5_u6_3x3_PRI_'), 10)

	labels = ['HO-PRI mean', 'HO-PRI best', 'HO-CCGA mean', 'HO-CCGA best', 'Schedulable']
	compare('AVA Breakdown Frequency', ho_pri_ava, ho_ccga_ava, labels, BDF_COL, max_x=50, min_y=0.5, max_y=1.5)
	# compare('AVA Schedulability', ho_pri_ava, ho_ccga_ava, labels, SCHED_COL, max_x=50, low_best=False)
	compare('Generated - 0.25 Breakdown Frequency', ho_pri_25, ho_ccga_25, labels, BDF_COL, max_x=50, max_y=1.5)
	# compare('Generated - 0.25 Schedulability', ho_pri_25, ho_ccga_25, labels, SCHED_COL, max_x=50, low_best=False)
	compare('Generated - 0.5 Breakdown Frequency', ho_pri_5, ho_ccga_5, labels, BDF_COL, max_x=50, max_y=1.5)
	# compare('Generated - 0.5 Schedulability', ho_pri_5, ho_ccga_5, labels, SCHED_COL, max_x=50, low_best=False)

	het_s_ava = read_files(path.join('ccga_ava_3x3_HET_S', 'ccga_ava_3x3_HET_S_'), 10)

	labels = ['HET-STATIC mean', 'HET-STATIC best', 'HO-CCGA mean', 'HO-CCGA best', 'Schedulable']
	compare('AVA Breakdown Frequency', het_s_ava, ho_ccga_ava, labels, BDF_COL, max_x=50, min_y=0.5, max_y=1.5)
	# compare('AVA Schedulability', het_s_ava, ho_ccga_ava, labels, SCHED_COL, max_x=50, low_best=False)



