# Questions:

## Colors in cells
Each cell has a value

- What values are possible? 

=> I suppose one should pick at least four (four-color theorem)

## Initial board
The task is to implement a program which fills a board with random
values, somehow making sure that there are no clusters. Print this board
on the console, using ASCII graphics like the above.

- In what circumstances is it NOT possible to fill a board free of clusters?

=> At least it seems it should always be possible to fill a board free of
clusters given that one controls which color to put in a cell.

=> Given that we select at least four colors for the cells, it is always
possible to fill a board free of clusters.


## Max points (greedy choice of rotation)
then it should find the point to rotate around which gives the maximum
points in total. do this rotation, print the resulting board along with
the summed score up till now.

- What does 'maxium points in total' mean?

=> It can't really mean points after removing initial cluster(s) and
subseqeuent appearing clusters because the subsequent clusters can't really be
predicted because the new cells that drop down in the board are selected at
random.

=> On the other hand, we can certainly predict how many points may be scored
from the cells already in the board which may drop in subsequent moves.

=> In general, a greedy choice may not lead to the highest sum of points across
all moves.

## Forming of clusters
Notice that a cluster of 4 (or more) might be formed:

- What constitutes a cluster?

=> One could image (at least) two ways to define a cluster:

1) "max. number of same-valued cells adjacent to each other"

2) like 1) but in a formation where each cell has at least two adjacent
   same-valued cells

=> It seems the common interpretation is 2) from the games available to play
(web + android)
