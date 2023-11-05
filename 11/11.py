class Monkey():

    def __init__(self, lines: [str], devideByThree: bool) -> None:

        self.index:int = 0
        self.items:[int] = []
        self.operation = lambda x : x
        self.testDivider:int = 0
        self.testPositiveMonkey:int = 0
        self.testNegativeMonkey:int = 0
        self.inspectCount:int = 0
        self.devideByThree = devideByThree

        self.index = int(lines[0].strip().split(" ")[1].split(":")[0])
        for item in lines[1].strip().split(":")[1].split(","):
            self.items.append(int(item))
        operationStr:str = lines[2].strip().split("= ")[1]
        if operationStr.startswith("old * old"):
            self.operation = lambda x : x * x
        elif "*" in operationStr:
            factor = int(operationStr.split("*")[1])
            self.operation = lambda x : x * factor
        elif "+" in operationStr:
            summand = int(operationStr.split("+")[1])
            self.operation = lambda x : x + summand
        self.testDivider = int(lines[3].strip().split(" ")[-1])
        self.testPositiveMonkey = int(lines[4].strip().split(" ")[-1])
        self.testNegativeMonkey = int(lines[5].strip().split(" ")[-1])
    
    def __str__(self):
        output = "Monkey %d" % self.index + "\n"
        output += "  Items: " + str(self.items) + "\n"
        return output
    
    def hasNext(self) -> bool:
        return len(self.items) > 0

    def inspectNextItem(self) -> (int, int):
        item = self.items.pop(0)
        #print("Monkey inspects an item with a worry level of %d." % item)
        item = self.operation(item)
        #print("New worry level %d." % item)
        self.inspectCount += 1
        if self.devideByThree:
            item = item // 3
        #print("Devided by 3 to %d." % item)
        if item % self.testDivider == 0:
            #print("Current worry level is not divisible by %d." % self.testDivider)
            #print("Item with worry level %d is thrown to monkey %d." % (item, self.testPositiveMonkey))
            return (self.testPositiveMonkey, item)
        else:
            #print("Current worry level is divisible by %d." % self.testDivider)
            #print("Item with worry level %d is thrown to monkey %d." % (item, self.testNegativeMonkey))
            return (self.testNegativeMonkey, item)
    
    def catchItem(self, item):
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
        #print("Monkey %d" % monkey.index)
        while monkey.hasNext():
            monkeyIndex, item = monkey.inspectNextItem()
            #print(monkey)
            #input()
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
    #taskOne()
    taskTwo()

def taskOne():
    monkeys = Monkeys()
    for i in range(20):
        monkeys.round()
    nrOfInspections = sorted(monkeys.nrOfInspections())
    product = nrOfInspections[-1] * nrOfInspections[-2]
    print(product)

def taskTwo():
    monkeys = Monkeys(False)
    for i in range(100):
        monkeys.round()
    print(monkeys)
    print(monkeys.nrOfInspections())
    nrOfInspections = sorted(monkeys.nrOfInspections())
    product = nrOfInspections[-1] * nrOfInspections[-2]
    print(product)



if __name__ == "__main__":
    main()

