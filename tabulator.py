import xlrd
import csv
import os
import re

# Python 3.4

def cell_finder(some_str, some_sheet):
    for row in range (some_sheet.nrows):
        for column in range(some_sheet.ncols):
            if some_str == some_sheet.cell(row, column).value:
                return (row, column)

header_row = ["county", "precinct", "office",
             "district", "party", "candidate", "votes"]

with open("20181106__ne__general__precinct.csv", "w") as csv_file:
    writer = csv.writer(csv_file, quoting=csv.QUOTE_NONNUMERIC)
    writer.writerow(header_row)

    all_files = os.listdir(".")

    # Don't grab the .py or .txt file in this directory

    files_to_tabulate = []
    for n in range(len(all_files)):
        if "xls" in all_files[n]:
            files_to_tabulate.append(all_files[n])

    # Loop through the filenames.
    # Grab the precinct and the office
    # from the filename

    for p in range(len(files_to_tabulate)):

        filename = files_to_tabulate[p]
        filename = filename.replace('.xls','')
        print(filename)
        bits = filename.split('__')
        if len(bits) > 4:
            district = bits[4]
            office = bits[3].replace("_", " ").title()
        else:
            district = None
            office = bits[3].replace("_", " ").title()

        # then open the file

        book = xlrd.open_workbook(files_to_tabulate[p])
        results = []

        # grab only the sheets with a county name.
        # dont get the grand total sheets or the
        # breakdown by absentee or whatever.

        sheet_names = book.sheet_names()

        # With that list of county names, aka sheets,
        # loop through each one and grab the votes

        for county in sheet_names:
            if county == 'County Results':
                continue
            sh = book.sheet_by_name(county)

            # get clean county name
            county_raw = sh.name
            county_split = county_raw.split("-")

            candidates = [x.strip() for x in sh.row_values(5)[1:-1]]

            for row in range(6, sh.nrows):
                row_values = sh.row_values(row)
                precinct = row_values[0]
                if precinct == 'TOTAL':
                    break
                precinct = precinct.replace('-','~')
                cand_values = zip(candidates, row_values[1:-1])
                for candidate, votes in cand_values:
                    new_row = [county, precinct, office, district, None, candidate, votes]
                    writer.writerow(new_row)
