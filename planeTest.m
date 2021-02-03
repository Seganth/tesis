

x=y=linspace(0,1,40)';
[xx,yy]=meshgrid(x,y);
z=1-xx-yy;

mesh(xx,yy,z);

##vectorx=[0 1]
##vectory=[1 0]
##vectorz=[0 0]
##plot3(vectorx,vectory,vectorz)

axis ([0,1,0,1,0,1]);

xlabel("x");
ylabel("y");
zlabel("z");
##title("Simplex en 3 dimensiones");