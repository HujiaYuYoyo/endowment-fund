import sys

filename = sys.argv[1]
new_file = open('formatted_' + filename,'w+')

seen_wealths = set()

with open(filename) as fp:
	while True:
		stock = fp.readline()
		cbond = fp.readline()
		gbond = fp.readline()
		alter = fp.readline()
		cash = fp.readline()

		if not stock:
			break

		wealth = float(stock.split(',')[1])
		if wealth not in seen_wealths:
			new_file.write(stock)
			new_file.write(cbond)
			new_file.write(gbond)
			new_file.write(alter)
			new_file.write(cash)
			seen_wealths.add(wealth)


new_file.close()