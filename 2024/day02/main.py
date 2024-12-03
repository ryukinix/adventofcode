def signum(x):
    # holy christ: why python don't have this no stdlib?!
    return (x > 0) - (x < 0)


def safe_line(line: list[int]) -> bool:
    checksum_target = len(line) - 1
    checksum = [
        signum(a - b) if abs(a - b) <= 3 else 0
        for a, b in zip(line, line[1:])
    ]
    return abs(sum(checksum)) == checksum_target


def main():
    lines = [list(map(int, line.split()))
             for line in open("input.txt").readlines()]
    print("part(a) | safe_lines: ", len(list(filter(safe_line, lines))))


if __name__ == "__main__":
    main()
