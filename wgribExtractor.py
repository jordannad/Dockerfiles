#!/usr/bin/env python
import argparse
import csv
import os
import subprocess

# Potential Inventory to use for extraction (check before use)
#WGRIB_INPUT = """1:0:d=04093012:PRES:sfc:anl:NAve=4
#2:88:d=04093012:TMP:2 m above gnd:anl:NAve=4
#3:174:d=04093012:UGRD:10 m above gnd:anl:NAve=4
#4:260:d=04093012:VGRD:10 m above gnd:anl:NAve=4
#5:346:d=04093012:SPFH:2 m above gnd:anl:NAve=4
#6:436:d=04093011:APCP:sfc:0-1hr acc:NAve=4
#9:520:d=04093012:DSWRF:sfc:anl:NAve=4
#10:610:d=04093012:DLWRF:sfc:anl:NAve=4"""

ORDER_MAPPING = [
  7, 8, 6, 2, 3, 4, 1, 5,
]

# Extracted text output written to this file
TMP_OUTPUT_FILE = '/tmp/foobar'
# Full path to wgrib binary used to extract the grib files
WGRIB_BINARY = '/usr/local/bin/wgrib'

def process_file(filename, outfile):
  '''
  Executes wgrib to extract the data points.
  '''
  gen_command = [
   WGRIB_BINARY,
   '-s',
   filename,
  ]
  print "Executing generation command: %s" % ' '.join(gen_command)
  gen_output = subprocess.Popen(gen_command, stdout=subprocess.PIPE)
  cmd = [
    WGRIB_BINARY,
    '-i',
    '-text',
    filename,
    '-o',
    TMP_OUTPUT_FILE,
  ]
  print "Filename: %s" % filename
 # print "About to execute command: %s" % ' '.join(cmd)
  wgrib_proc = subprocess.Popen(cmd, stdin=gen_output.stdout, stdout=subprocess.PIPE) 
  extraction_output = wgrib_proc.communicate()[0]
 # print "Extraction output:\n%s" % extraction_output
  process_wgrib_output(outfile)


def process_wgrib_output(outfile):
  '''
  Reads the scontents of TMP_OUTPUT_FILE and writes them to outfile
  in the appropriate format.
  '''
  tmpfile = open(TMP_OUTPUT_FILE, 'r')
  datalines = tmpfile.readlines()
  coordinates = [int(i) for i in datalines[0].split()]
  assert len(coordinates) == 2  # Sanity check
  num_rows = coordinates[0] * coordinates[1]
  output_row = []
  for i in range(0, len(datalines), num_rows+1):
    # Sanity check: make sure coordinates same for all data types
    assert datalines[0] == datalines[i], "Row %s: Got %s expected %s" % (i, datalines[i], datalines[0])
    output_row.append(datalines[i+1].strip())  # Pick 1st one
  ordered_row = []
  for index in ORDER_MAPPING:
    ordered_row.append(output_row[index - 1])
  outfile.writerow(ordered_row)


def parser():
  p = argparse.ArgumentParser(description='Convert to parflow?')
  p.add_argument('source_dir', help='Source directory of grib input files')
  p.add_argument('output_file', type=argparse.FileType('wb'), help='Output tsv file')
  return p
  

def main():
  'Executes the program'
  args = parser().parse_args()
  all_files = os.listdir(args.source_dir)
  grb_files = [i for i in all_files if i.endswith('.grb')]
  grb_files.sort()  # Must be in-order!
  outfile = csv.writer(args.output_file, delimiter='\t')
  for f in grb_files:
    process_file(args.source_dir + '/' + f, outfile)
  args.output_file.close()
  

if __name__ == '__main__':
  main()
 
