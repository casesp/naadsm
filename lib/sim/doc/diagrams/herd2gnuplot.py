#!/usr/bin/env python
"""This script turns a herd file in XML format into gnuplot commands that will
plot a diagram of the herds.

Susceptible herds are represented by white dots, sized to indicate the number
of animals.  Incubating (source) herds are yellow.
"""

__author__ = "Neil Harvey <neilharvey@canada.com>"
__date__ = "December 2003"

import re
import string
import sys
from math import pi, ceil, floor
from xml.dom.minidom import parse

EARTH_RADIUS = 6378.137
DEG2KM = EARTH_RADIUS * pi / 180
EPSILON = 0.001



class Herd:
	def __init__ (self, size, x, y, state, herd_id):
		self.size = size
		self.x = x
		self.y = y
		self.state = state
		self.id = herd_id


		
def unique (l):
	set = []
	for item in l:
		if item not in set:
			set.append (item)
	return set



def getText (node):
	text = ""
	for child in node.childNodes:
		if child.nodeType == child.TEXT_NODE or child.nodeType == child.CDATA_SECTION_NODE:
			text += child.data
	return text.strip()



def main ():
	herds = []

	doc = parse (sys.stdin)
	statepats = [
	  re.compile (r"0|Susceptible", re.I),
	  re.compile (r"1|Incubating|Latent", re.I),
	  re.compile (r"Infectious ?Subclinical|Inapparent ?Shedding", re.I),
	  re.compile (r"2|Infectious ?Clinical", re.I),
	  re.compile (r"3|Naturally ?Immune", re.I),
	  re.compile (r"4|Vaccine ?Immune", re.I),
	  re.compile (r"5|Destroyed|Dead", re.I)
	]

	# Read the herds.
	count = 0
	for herd in doc.getElementsByTagName ("herd"):
		idtags = herd.getElementsByTagName ("id")
		if len (idtags) > 0:
			herd_id = getText (idtags[0])
		else:
			herd_id = str (count)
		size = int (getText (herd.getElementsByTagName ("size")[0]))
		lat = float (getText (herd.getElementsByTagName ("latitude")[0]))
		lon = float (getText (herd.getElementsByTagName ("longitude")[0]))
		state = getText (herd.getElementsByTagName ("status")[0])
		for i in range (len(statepats)):
			match = statepats[i].match (state)
			if match:
				state = i
				break		
		herds.append (Herd (size, lon * DEG2KM, lat * DEG2KM, state, herd_id))
		count += 1

	bbox = [
	  min ([herd.x for herd in herds]),
	  min ([herd.y for herd in herds]),
	  max ([herd.x for herd in herds]),
	  max ([herd.y for herd in herds])
	]

	# Set an encoding that can handle letters with accents.  Gnuplot does not
	# offer Unicode encodings.
	print "set encoding iso_8859_1"

	# Set the x and y ranges.  First expand the bounding box to provide some
	# space around the points.  Then force the plot to be a square.
	bbox = [
	  floor (bbox[0] - 0.5),
	  floor (bbox[1] - 0.5),
	  ceil (bbox[2] + 0.5),
	  ceil (bbox[3] + 0.5)
	]
	xrange = bbox[2] - bbox[0]
	yrange = bbox[3] - bbox[1]
	diff = xrange - yrange
	if diff > 0: # box is wider than it is tall, increase height
		diff = diff / 2.0
		bbox[1] -= diff
		bbox[3] += diff
	elif diff < 0: # box is taller than it is wide, increase the width
		diff = diff / -2.0
		bbox[0] -= diff
		bbox[2] += diff
	print "set xrange [%g:%g]" % (bbox[0], bbox[2])
	print "set yrange [%g:%g]" % (bbox[1], bbox[3])
	print """\
set size square
set xlabel "km (E-W)"
set ylabel "km (N-S)"\
"""

	# Draw equator and prime meridian.
	print "set arrow from %g,%g to %g,%g nohead lt 0" \
	  % (bbox[0], 0, bbox[2], 0)
	print "set arrow from %g,%g to %g,%g nohead lt 0" \
	  % (0, bbox[1], 0, bbox[3])

	# Get the number of animals in the smallest and largest herds.  If herds
	# have different sizes, the size range will be linearly mapped into
	# [1.0,3.0] to get gnuplot point sizes.
	minsize = min ([herd.size for herd in herds])
	maxsize = max ([herd.size for herd in herds])
	sizerange = abs (maxsize - minsize)
	if sizerange < EPSILON:
		sizerange = 0

	# Write herd IDs near the points.
	offset = min (bbox[2] - bbox[0], bbox[3] - bbox[1]) / 30.0
	for herd in herds:
		print """set label "%s" at %g,%g left""" \
		  % (herd.id.encode("iso-8859-1", "replace"), herd.x + offset, herd.y - offset)

	# Colour comes from the line type in gnuplot.  Create an array of line
	# types for the colours in the herd state-transition diagram.
	linetype = [7,6,8,1,2,3,7,7]

	# Create an array of point types for dot shapes.
	pointtype = [71,7,7,7,7,7,7,7]

	# Create array of text description for states.
	statename = ["Susceptible", "Incubating", "Infectious Subclinical",
	  "Infectious Clinical", "Naturally immune", "Vaccine Immune", "Destroyed"]

	# Create commands to plot the herds.
	command = []
	for herd in herds:
		if sizerange == 0:
			pointsize = 2.0
		else:
			pointsize = 2.0 * (herd.size - minsize) / sizerange + 1.0
		command.append ("'-' notitle w p lt %i pt %i ps %g"
		  % (linetype[herd.state], pointtype[herd.state], pointsize))

	# Add commands to draw (off-screen) lines just to get a legend.
	#for state in unique ([state for size, x, y, state, herd_id in herds]):
	#	command.append ("""%g title "%s" w l lt %i"""
	#	  % (bbox[1] - 1, statename[state], linetype[state]))

	command = "plot " + string.join (command, ", \\\n")
	print command
	for herd in herds:
		print "%g %g\ne" % (herd.x, herd.y)



if __name__ == "__main__":
	main()
