f=open("data.csv","rw")
data=f.read()
num = data.split('\n')

def fuel(fl):
    extra_fuel = ((fl / 3) -2)
    if extra_fuel > 0:
        return extra_fuel + fuel(extra_fuel)
    return 0

print sum ([fuel(int(x)) for x in num])