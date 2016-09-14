ax=1; ay=1
bx=4; by=5
cx=2; cy=.9
px=-1; py=4

run <- function(px, py){
plot(c(ax,bx,cx),c(ay,by,cy), xlim=c(-1,6), ylim=c(-2, 6), pch=letters[1:3])
points(px,py,pch=20)
abline(lm(c(ay,by) ~ c(ax, bx)))
abline(lm(c(ay,cy) ~ c(ax, cx)))
abline(lm(c(cy,by) ~ c(cx, bx)))

#get barycentric coordinates
d = ((by-cy)*(px-cx) + (cx-bx)*(py-cy)) /
    ((by-cy)*(ax-cx) + (cx-bx)*(ay-cy))
e = ((cy-ay)*(px-cx) + (ax-cx)*(py-cy)) /
    ((by-cy)*(ax-cx) + (cx-bx)*(ay-cy))
f = 1-d-e

# d;e;f

#get slopes and intercepts of each triangle edge
mab = (ay-by)/(ax-bx)
mbc = (by-cy)/(bx-cx)
mac = (ay-cy)/(ax-cx)
iab = -mab*ax+ay
ibc = -mbc*bx+by
iac = -mac*ax+ay
#slopes for their perpendiculars
mabP = -1/mab
mbcP = -1/mbc
macP = -1/mac
#and intercepts for the perpendiculars passing through the consumer
iabP = -mabP*px+py
ibcP = -mbcP*px+py
iacP = -macP*px+py

#plot all perpendiculars (b=-mx+y), and m-perp is -1/m
abline(b=-1/mab, a=(1/mab)*ax+ay, col='lightgreen', lty=2)
abline(b=-1/mab, a=(1/mab)*bx+by, col='lightgreen', lty=2)
abline(b=-1/mbc, a=(1/mbc)*bx+by, col='lightblue', lty=2)
abline(b=-1/mbc, a=(1/mbc)*cx+cy, col='lightblue', lty=2)
abline(b=-1/mac, a=(1/mac)*cx+cy, col='palevioletred', lty=2)
abline(b=-1/mac, a=(1/mac)*ax+ay, col='palevioletred', lty=2)

# py < (-1/mab)*px - ax*(-1/mab) + ay #does p lie on the same side of both perpendiculars to each edge's endpoints?
# py < (-1/mab)*px - bx*(-1/mab) + by
# py < (-1/mbc)*px - bx*(-1/mbc) + by
# py < (-1/mbc)*px - cx*(-1/mbc) + cy
# py < (-1/mac)*px - ax*(-1/mac) + ay
# py < (-1/mac)*px - cx*(-1/mac) + cy

ifelse(d >= 0 && d <= 1 && e >= 0 && e <= 1 && f >= 0 && f <= 1,
    return('woot'), #consumer lies within the source triangle
    ifelse(e < 0 && f < 0,
        points(ax, ay, pch=1, cex=3), #in 'penumbra' of segment bc, so clamp to vertex a
        ifelse(f < 0 && d < 0,
            points(bx, by, pch=1, cex=3), #clamp to b
            ifelse(e < 0 && d < 0,
                points(cx, cy, pch=1, cex=3), #clamp to c
                ifelse(d > 0 && e > 0 && f < 0, #these regions clamp to either endpoint or edge of ab
                    ifelse(py < mabP*px - ax*mabP + ay && py < mabP*px - bx*mabP + by,
                        ifelse(ay < mabP*ax - bx*mabP + by,
                            points(ax, ay, pch=1, cex=3), #below both endpoint perpendiculars, so clamp
                            points(bx, by, pch=1, cex=3)), #to one endpoint or the other
                        ifelse(py > mabP*px - ax*mabP + ay && py > mabP*px - bx*mabP + by,
                            ifelse(ay > mabP*ax - bx*mabP + by,
                                points(ax, ay, pch=1, cex=3), #above both endpoint perpendiculars,
                                points(bx, by, pch=1, cex=3)), #so same deal
                            points(x=(iab - iabP)/(-mab + mabP), y=(-mab*iabP + mabP*iab)/(-mab + mabP), pch=1, cex=3))), #clamp to nearest point on ab

                    ifelse(d < 0 && e > 0 && f > 0, #same thing for bc
                        ifelse(py < mbcP*px - bx*mbcP + by && py < mbcP*px - cx*mbcP + cy,
                            ifelse(by < mbcP*bx - cx*mbcP + cy,
                                points(bx, by, pch=1, cex=3),
                                points(cx, cy, pch=1, cex=3)),
                            ifelse(py > mbcP*px - bx*mbcP + by && py > mbcP*px - cx*mbcP + cy,
                                ifelse(by > mbcP*bx - cx*mbcP + cy,
                                    points(bx, by, pch=1, cex=3),
                                    points(cx, cy, pch=1, cex=3)),
                                points(x=(ibc - ibcP)/(-mbc + mbcP), y=(-mbc*ibcP + mbcP*ibc)/(-mbc + mbcP), pch=1, cex=3))),

                        ifelse(d > 0 && e < 0 && f > 0, #same thing for ac
                            ifelse(py < macP*px - ax*macP + ay && py < macP*px - cx*macP + cy,
                                ifelse(ay < macP*ax - cx*macP + cy,
                                    points(ax, ay, pch=1, cex=3),
                                    points(cx, cy, pch=1, cex=3)),
                                ifelse(py > macP*px - ax*macP + ay && py > macP*px - cx*macP + cy,
                                    ifelse(ay > macP*ax - cx*macP + cy,
                                        points(ax, ay, pch=1, cex=3),
                                        points(cx, cy, pch=1, cex=3)),
                                    points(x=(iac - iacP)/(-mac + macP), y=(-mac*iacP + macP*iac)/(-mac + macP), pch=1, cex=3))))))))))
}

run(-1,0)
run(1,4)
run(1.5,3)
run(2,-2)
run(1,-2)
run(6,0)
run(6,6)
run(4.6,6)
run(2,2)


ax=1; ay=1
bx=5; by=1.1
cx=2.5; cy=5

run(3,3)
run(-1,-2)
run(6,1.4)
run(6,2)
