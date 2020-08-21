import subprocess
from datetime import datetime

filename = 'ip_log.txt'
try:
	ip = subprocess.check_output('curl inet-ip.info',shell = True)

	if(ip[0:7] != '203.138' and ip[0:7] != '203.149' and ip[0:7] != '203.180'):
		subprocess.check_output('sudo candy service restart', shell = True)

	lastlog = subprocess.check_output('tail -1 ip_log.txt', shell = True)
	if(lastlog.split('\t')[1] != ip):
		with open(filename,'a') as fw:
			fw.write(datetime.now().strftime('%F %T') + '	' + ip)

except:
	''
exit()
