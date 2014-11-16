#
# AnalyzeMap.py
# Scans a map for special markers
#
# Jonatan H Sundqvist
# November 2 2014
#

# TODO | - Fill with triangles (tiling)
#        - 
#        - Alignment cues, snap-to-greed
#        - User canvas overlay instead (?)

# SPEC | -
#        -



from tkinter import *
from os import listdir
from os.path import splitext
from PIL import Image, ImageTk, ImageDraw



def findMarkers(fn, colour):

	'''
	Finds all markers with the specified colour
	in the image indicated by the filename.

	'''

	image = Image.open(fn)
	pixels = image.load()
	w, h = image.size

	return [(x,y) for x in range(w) for y in range(h) if pixels [x, y] == colour]



def silhouette(fn, colour):

	'''
	Docstring goes here
	
	'''

	return None



def placeTiles(root, fn, colour):

	'''
	Docstring goes here

	'''

	return None



def placeMarkers(root, fn, colour):

	'''
	Interactively placing markers on
	an image and returning their coordinates

	'''

	def onClick(event):
		# onSubmit
		markers.append((event.x, event.y))
		draw = ImageDraw.Draw(label.image)
		draw.ellipse((event.x-5, event.y-5, event.x+5, event.y+5), fill='green')
		label.photo = ImageTk.PhotoImage(label.image)
		label.config(image=label.photo)

	def onMove(event):
		coords.config(text='X %d | Y %d' % (event.x, event.y))

	def onFinished(*args):
		saveMarkers(name.get(), markers[:-1])
		# del markers[:]

	image = Image.open(fn)
	w, h = image.size
	image.thumbnail((int(w*0.8), int(h*0.8)), Image.ANTIALIAS)
	photo = ImageTk.PhotoImage(image)
	label = Label(image=photo)
	label.photo = photo
	label.image = image
	label.place(x=0, y=0, relwidth=1, relheight=1)	
	# label.pack()

	coords = Label(text="X: Y: ")
	coords.grid(row=0, column=2)

	button = Button(text='Finished', command=onFinished)
	button.grid(row=0, column=0)

	name = Entry(text='filename.txt')
	name.grid(row=0, column=1)

	root.bind('<Motion>', onMove)
	root.bind('<Button-1>', onClick)
	root.geometry('%dx%d' % image.size)
	markers = []

	return markers



def loadImages(directory, root):
	
	'''
	Docstring goes here.

	'''

	files = [fn for fn in listdir(directory) if splitext(fn)[-1] in ['.png']]
	images = { fn : ImageTk.PhotoImage(Image.open(fn)) for fn in files}
	menu = OptionMenu(root, StringVar(value=files[0]), *files)
	menu.grid(row=0, column=3)
	return images



def saveMarkers(fn, markers):
	
	'''
	Saves marker data to file

	'''

	with open(fn, 'w+') as f:
		f.write(';'.join('%d,%d' % marker for marker in markers))



def main():
	
	'''
	Docstring goes here
	
	'''

	root = Tk()
	root.title('Place markers')

	markers = placeMarkers(root, 'sweden_outline.png', (34, 177, 76, 255))
	images = loadImages('.', root)
	root.mainloop()

	# markers = findMarkers('sweden_outline.png', (34, 177, 76, 255))
	# print(markers)
	# print(len(markers))

	# saveMarkers('generated.txt')



if __name__ == '__main__':
	main()