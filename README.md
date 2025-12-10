# Advent of Code in F#

[AdventOfCode](https://adventofcode.com/)

## Adding a Solution

1. Install the Solution template by running `dotnet new --install fs-solution-template`. This only needs to be run once locally.

2. Add a solution file and input by running `dotnet new fs-solution-template --year <year> --day <day>` where year is the year of the solution and day is the day. For example to create the files for Day 1 of 2025 run `dotnet new fs-solution-template --year 2025 --day 1`. This will create a solution file at `Solutions/2025/Day1.fs` and an input file at `Input/2025/Day1.txt`

3. Copy the input into the input file.

## Running a solution

### Debugging

1. Update `.vscode/launch.json` to set the args to the current solution. 

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            ...
            "args": ["2025.1.1"],
            ...
        }
    ]
}
```
2. Run and debug `.NET Core Launch and Debug`

### Build and Run

1. Run `dotnet build`
2. Run `dotnet run 2025.1.1`

