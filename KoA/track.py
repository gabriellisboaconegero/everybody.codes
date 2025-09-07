# Everybody Codes - Day 7, Year 2024
# Solved in 2024
# Puzzle Link: https://everybody.codes/event/2024/quests/7
# Solution by: [abbasmoosajee07]
# Brief: [Race Tracks and Fights]

#!/usr/bin/env python3

import os, re, copy, itertools

# List of input file names
input_files = ["input-7-3.txt"]

# Read and split the input data into individual lists
track_p3_txt = open(os.path.join(os.path.dirname(__file__), input_files[0])).read().strip().split('\n')

# Now, input_data_p1, input_data_p2, input_data_p3 contain the respective data
# print(input_data_p1), print(input_data_p2), print(race_track), print(input_data_p3)
def ranking_grid(input_data, loops = 1):
    rank_grid = []
    for row in input_data:
        row_data = row.split(':')
        start_rank = 10
        row_rank = []
        ranks = row_data[1].split(',')

        for _ in range(loops):  # Loop `loops` times
            for i in range(len(ranks)):
                rank = ranks[i % len(ranks)]  # Circular iteration
                if rank == '+':
                    start_rank += 1
                elif rank == '-':
                    start_rank -= 1
                elif rank == '=':
                    start_rank = start_rank  # No change for '='
                row_rank.append(start_rank)

        rank_grid.append([row_data[0], sum(row_rank)])
    rank_array = np.array(rank_grid)

    # Sort the second column (ranking) in descending order
    sorted_indices = np.argsort(rank_array[:, 1])[::-1]
    sorted_rankings = rank_array[sorted_indices, 0]  # Reordered first column (knights)
    return ''.join(sorted_rankings), rank_array

def flatten_race_track(race_track_array):
    race_start = ''.join(race_track_array[0][1:])
    race_end = ''.join(race_track_array[-1][::-1])
    race_track = [race_start]
    line_end = []
    for row_no in range(1, len(race_track_array) - 1):
        current_track = race_track_array[row_no]
        race_track.append(current_track[-1])
        line_end.append(current_track[0])
    line_end.append('S')
    race_track.append(race_end)
    race_track += line_end
    return ''.join(race_track)

def create_race_track(track_map):
    rows, cols = len(track_map), len(track_map[0])
    row, col, direction = 0, 0, 1
    directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]  # N, E, S, W
    path = ''
    is_done = False
    steps = 0
    while not is_done:
        if len(path) > 0 and track_map[row][col] == 'S':
            break
        if is_done:
            break
        for new_direction in [direction, (direction + 3) % 4, (direction + 1) % 4]:
            new_row = row + directions[new_direction][1]
            new_col = col + directions[new_direction][0]
            if 0 <= new_row < len(track_map) and 0 <= new_col < len(track_map[new_row]) and track_map[new_row][new_col] != ' ':
                path += track_map[new_row][new_col]
                row, col = new_row, new_col
                direction = new_direction
                steps += 1
                break
        if track_map[row][col] == 'S':
            is_done = True
    return path
track_str = create_race_track(track_p3_txt)

print(f"Quest 3: {track_str}")