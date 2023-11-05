from typing_extensions import Self

primes:[int] = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]

class MonkeyItem():
    def __init__(self, number: int) -> None:
        self.moduloCounter = list(map(lambda p : number % p, primes))
    
    def powerOfTwo(self) -> Self: 
        for i in range(len(primes)):
            self.moduloCounter[i] = (self.moduloCounter[i] ** 2) % primes[i]
    
    def mult(self, factor: int) -> Self:
        for i in range(len(primes)):
            self.moduloCounter[i] = (self.moduloCounter[i] * factor) % primes[i]

    def add(self, summand: int) -> Self:
        for i in range(len(primes)):
            self.moduloCounter[i] = (self.moduloCounter[i] + summand) % primes[i]
    
    def devisibleBy(self, number: int) -> bool:
        assert number in primes
        return self.moduloCounter[primes.index(number)] == 0
    
    def __str__(self) -> str:
        return str(self.moduloCounter)

class Monkey():
    def __init__(self, lines: [str], devideByThree: bool) -> None:
        self.index:int = 0
        self.items:[MonkeyItem] = []
        self.operation = None
        self.testDivider:int = 0
        self.testPositiveMonkey:int = 0
        self.testNegativeMonkey:int = 0
        self.inspectCount:int = 0
        self.devideByThree = devideByThree

        self.index = int(lines[0].strip().split(" ")[1].split(":")[0])
        for number in lines[1].strip().split(":")[1].split(","):
            self.items.append(MonkeyItem(int(number)))
        operationStr:str = lines[2].strip().split("= ")[1]
        if operationStr.startswith("old * old"):
            self.operation = lambda x : x.powerOfTwo()
        elif "*" in operationStr:
            factor = int(operationStr.split("*")[1])
            self.operation = lambda x : x.mult(factor)
        elif "+" in operationStr:
            summand = int(operationStr.split("+")[1])
            self.operation = lambda x : x.add(summand)
        self.testDivider = int(lines[3].strip().split(" ")[-1])
        self.testPositiveMonkey = int(lines[4].strip().split(" ")[-1])
        self.testNegativeMonkey = int(lines[5].strip().split(" ")[-1])
    
    def __str__(self):
        output = "Monkey %d" % self.index + "\n"
        output += "  Items: " + str([item.__str__() for item in self.items]) + "\n"
        return output
    
    def hasNext(self) -> bool:
        return len(self.items) > 0

    def inspectNextItem(self) -> (int, MonkeyItem):
        item:MonkeyItem = self.items.pop(0)
        self.operation(item)
        self.inspectCount += 1
        if item.devisibleBy(self.testDivider):
            return (self.testPositiveMonkey, item)
        else:
            return (self.testNegativeMonkey, item)
    
    def catchItem(self, item: MonkeyItem):
        self.items.append(item)

class Monkeys():
    monkeys:[Monkey] = []

    def __init__(self, devideByThree = True) -> None:
        with open("input01.txt") as f:
            lines = f.readlines()
            nrOfMonkey = len(lines) // 7 + 1
            for i in range(nrOfMonkey):
                pos0 = i * 7
                monkey = Monkey(lines[pos0:pos0+7], devideByThree)
                self.monkeys.append(monkey)

    def turn(self, monkey: Monkey) -> None:
        while monkey.hasNext():
            monkeyIndex, item = monkey.inspectNextItem()
            self.monkeys[monkeyIndex].catchItem(item)
    
    def round(self) -> None:
        for monkey in self.monkeys:
            self.turn(monkey)

    def nrOfInspections(self) -> [int]:
        return list(map(lambda monkey : monkey.inspectCount, self.monkeys))

    def __str__(self) -> str:
        output = ""
        for monkey in self.monkeys:
            output += monkey.__str__()
            output += "\n"
        return output

def main():
    taskTwo()

def taskTwo():
    monkeys = Monkeys(False)
    for i in range(10000):
        monkeys.round()
    print(monkeys.nrOfInspections())
    nrOfInspections = sorted(monkeys.nrOfInspections())
    product = nrOfInspections[-1] * nrOfInspections[-2]
    print(product)

if __name__ == "__main__":
    main()

