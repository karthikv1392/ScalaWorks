#include <stdio.h>
#include <sys/types.h>
#include "ppmIO.h"
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "polygon.h"
#define RBIN 800
#define TBIN 720
#define PI 3.14159

void GetA(int p[2][2], int q[2][2], float A[2][2]);
int GetE(int p[2][4],int q[2][4]);
void MinE(point_t p[],int q[2][4],int npoints,int *besterror,int bestfour[4]);
int grad(int *grey, int rows, int cols, int *grad);
void getgray(Pixel *image, int rows, int cols, int *grey);
int getdx(int *grey, int rows, int cols);
int getdy(int *grey, int rows, int cols);
void getmodelpoints(char* filename,int q[2][4]);
int getreal(char* filename, char* prefix);
int GetCc(Pixel *image, Pixel *ccimage, int rows, int cols, int colors, point_t model[], char *name);

int GetCc(Pixel *im, Pixel *ccimage, int rows, int cols, int colors, point_t model[], char *name){
  long i, j, size, bbox[4];
  short *regmap;
  short threshold = 150;
  int cCode[5000], deriv[5000];
  polygon *p;
  int r = 0, c = 0, full, filtered;
  point_t points[5000];
  char *ccname;

  size = (long)rows * (long)cols;
  //  regmap = (short *)malloc(sizeof(short) * size);
  regmap = new short[size];

  // threshold the image
  for(i=0;i<size;i++) {
    if((int)im[i].r + (int)im[i].g + (int)im[i].b < threshold) {
      regmap[i] = 0;
      im[i].r = 160;
      im[i].g = im[i].b = 0;
    }
    else {
      regmap[i] = 1;
    }
  }

  for(i=0; i^lt;5000; i++){
    cCode[i]=-1;
    deriv[i]=-1;
  }
  printf("Writing thresholded image\n");
  writePPM(im, rows, cols, colors, "region.ppm");

  bbox[0] = bbox[1] = 0;
  bbox[2] = rows - 1;
  bbox[3] = cols -1;
  p = polygon_fitRegion(regmap, (long)rows, (long)cols, 1, bbox, cCode, &r, &c);

  ccimage[r*cols+c].r=255;
  ccimage[r*cols+c].g=255;
  ccimage[r*cols+c].b=0;

  points[0].row=r;
  points[0].col=c;

  for(i=0;i<size;i++)
    im[i].r = im[i].g = im[i].b = 0;

  for(i=0;i<p->nVertices;i++) {
    im[p->pRow[i]*cols + p->pCol[i]].b = 255;
    im[p->pRow[i]*cols + p->pCol[i]].g = 0;
    im[p->pRow[i]*cols + p->pCol[i]].r = 0;
  }

  i=0;
  while(cCode[i]!=-1){
    switch(cCode[i]){
    case 0:
      c++;
      break;
    case 1:
      c++;
        r--;
        break;
    case 2:
      r--;
      break;
    case 3:
      r--;
      c--;
      break;
    case 4:
      c--;
      break;
    case 5:
      r++;
      c--;
      break;
    case 6:
      r++;
      break;
    case 7:
     r++;
      c++;
      break;
    }
    ccimage[r*cols+c].r=255;
    ccimage[r*cols+c].g=255;
    ccimage[r*cols+c].b=0;

    points[i].row=r;
    points[i].col=c;

    i++;
    full=i;
  }
  full--;

  printf("Writing boundary image\n");
  writePPM(im, rows, cols, colors, "polygon.ppm");

  for(i=1; i<full;i++){
    deriv[i-1]=cCode[i]-cCode[i-1];
    if(deriv[i-1]<0)
      deriv[i-1]+=8;
  }

  deriv[full-1]=cCode[0]-cCode[full];
  i=0;
  j=0;
  while(deriv[i]!=-1){
    if((deriv[i]==7&&deriv[i+1]==7)||(deriv[i]==1&&deriv[i+1]==1)||deriv[i]==2){
      ccimage[points[i].row*cols+points[i].col].r=255;
      ccimage[points[i].row*cols+points[i].col].g=0;
      ccimage[points[i].row*cols+points[i].col].b=0;
      if(j<30){
        model[j].row=points[i].row;
        model[j].col=points[i].col;
        j++;
      }
    }
    i++;
  }

  printf("Writing chain code image\n");
  writePPM(ccimage, rows, cols, colors, name);
  return (j);
}


int getreal(char* filename, char* prefix)
{
  char string2[50];

  sscanf(filename, "%[^.]", string2);
  if(strcmp(string2, prefix)==0) return 0;
  return 1;
}

void GetA(int p[2][2], int q[2][2], float A[2][2])
{
  int det,i,j;
  det = (p[0][0]*p[1][1] - p[1][0]*p[0][1]);
  A[0][0] = (float)(p[1][1]*q[0][0] - q[0][1]*p[1][0])/det;
  A[0][1] = (float)(q[0][1]*p[0][0] - p[0][1]*q[0][0])/det;
  A[1][0] = (float)(q[1][0]*p[1][1] - q[1][1]*p[1][0])/det;
  A[1][1] = (float)(q[1][1]*p[0][0] - p[0][1]*q[1][0])/det;

}

int GetE(int p[2][4],int q[2][4])
{
  int pt[2][4],p23[2][2];
  int qt[2][4],q23[2][2];
  float A[2][2];
  int px4,py4,dx,dy,error;
  int i,j;
  for(i=0;i<2;i++){
    for(j=0;j<4;j++){
      pt[i][j] = p[i][j] - p[i][0];
      qt[i][j] = q[i][j] - q[i][0];
    }
  }

  for(i=0;i<2;i++){
    for(j=0;j<2;j++){
      p23[i][j] = pt[i][j+1];
      q23[i][j] = qt[i][j+1];
    }
  }

  GetA(p23,q23,A);
  px4 = A[0][0]*pt[0][3] + A[0][1]*pt[1][3];
  py4 = A[1][0]*pt[0][3] + A[1][1]*pt[1][3];
  dx = px4-qt[0][3];
  dy = py4-qt[1][3];
  return(sqrt(dx*dx+dy*dy));
}

void MinE(point_t p[], int q[2][4], int npoints, int *besterror, int bestfour[4])
{
 int i,j,k,l,m,error;
 int ps[2][4];
 *besterror = 800;
  for(i=0;i<4;i++) bestfour[i]=i;

  for(i=0;i<npoints;i++){
   for(j=0;j<npoints;j++){
     for(k=0;k<npoints;k++){
       for(l=0;l<npoints;l++){
         if(i !=j && i !=k && i !=l && j !=k && j != l && k!= l){
           ps[0][0] = p[i].row ;
           ps[0][1] = p[j].row;
           ps[0][2] = p[k].row;
           ps[0][3] = p[l].row;
           ps[1][0] = p[i].col;
           ps[1][1] = p[j].col;
           ps[1][2] = p[k].col;
           ps[1][3] = p[l].col;
           error = GetE(ps,q);
           //printf("error = %d\n",error);
           if (error < *besterror){
             bestfour[0]=i;
             bestfour[1]=j;
             bestfour[2]=k;
             bestfour[3]=l;
             *besterror = error;
           }
         }
       }
     }
   }
  }
}

void getgray(Pixel *image, int rows, int cols, int *grey)
{
  int i,j;
  for (i=0;i<rows;i++){
    for (j=0;j<cols;j++){
      grey[i*cols+j] = (image[i*cols+j].r + image[i*cols+j].g + image[i*cols+j].b)/3;
    }
  }
}

int getdx(int *grey, int rows, int cols, int i, int j)
{
  int dx;
  dx =(grey[(i-1)*cols+j+1] + 2*grey[i*cols+j+1] + grey[(i+1)*cols+j+1]) -(grey[(i-1)*cols+j-\
1]+2 * grey[i*cols+j-1]+grey[(i+1)*cols+j-1]);
  return(dx);
}

int getdy(int *grey, int rows, int cols, int i, int j)
{
  int dy;
  dy =(grey[(i+1)*cols+j-1] + 2*grey[(i+1)*cols+j] + grey[(i+1)*cols+j+1]) -(grey[(i-1)*cols+\
j-1]+2 * grey[(i-1)*cols+j] + grey[(i-1)*cols+j+1]);
  return(dy);
}

int grad(int *grey, int rows, int cols, int *dx,int *dy)
{
  int i,j,max,mag;
 max =0;
  for(i=1;i<rows-1;i++){
    for(j=1;j<cols -1;j++){
      dx[i*cols+j] = getdx(grey,rows,cols, i, j);
      dy[i*cols+j] = getdy(grey,rows,cols, i, j);
      mag = dx[i*cols+j]*dx[i*cols+j]+dy[i*cols+j]*dy[i*cols+j];
      if (mag > max) max = mag;
    }
  }
  return (max);
}

void getmodelpoints(char* filename,int q[2][4])
{
  FILE *fp;

  fp = fopen(filename, "r");

  fscanf(fp, "%d %d %d %d\n", &q[0][0], &q[0][1], &q[0][2], &q[0][3]);
  fscanf(fp, "%d %d %d %d\n", &q[1][0], &q[1][1], &q[1][2], &q[1][3]);
  fclose(fp);
  return;
}

main(int argc, char *argv[]) {
  Pixel *image, *ccimage;
  int rows, cols, colors, rows2, cols2, colors2, npoints;
  int *dx, *dy, *grey;
  int H[RBIN][TBIN];
  int i,j,k,I,r,d,max,m;
  int q[2][4],bestfour[4],besterror;
  int mine, real, confusion[2][2];
  char gradfname[50];
  point_t model[30];

  printf("Beginning process\n");

  confusion[0][0]=0;
  confusion[0][1]=0;
  confusion[1][0]=0;
  confusion[1][1]=0;

  for(m=4;m<argc;m++){
    image = readPPM(&rows, &cols, &colors, argv[m]);
    ccimage = readPPM(&rows2, &cols2, &colors2, argv[m]);
    printf("Image=%s\n", argv[m]);

    for(i=0; i<rows2*cols2; i++){
      ccimage[i].r=0;
      ccimage[i].g=0;
      ccimage[i].b=0;
    }

    npoints=GetCc(image, ccimage, rows, cols, colors, model, argv[3]);

    printf("Image is %d rows by %d columns\n", rows, cols);

    grey = (int*) malloc(rows*cols*sizeof(int));
    dx = (int*) malloc(rows*cols*sizeof(int));
    dy = (int*) malloc(rows*cols*sizeof(int));


    for (i=0;i<rows;i++){
      for (j=0;j<cols;j++){
        if(dx[i*cols+j]*dx[i*cols+j]+dy[i*cols+j]*dy[i*cols+j]< max/40){
          image[i*cols+j].r= 255;
          image[i*cols+j].g= 255;
          image[i*cols+j].b= 255;
        }
        else{
          image[i*cols+j].r= 0;
          image[i*cols+j].g= 0;
          image[i*cols+j].b= 0;
        }
      }
    }

    printf("points = %d\n",npoints);

    getmodelpoints(argv[1],q);

    //    for(j=0; j<npoints; j++)
    // printf("point[%d]=(%d, %d)\n", j, model[j].row, model[j].col);

    MinE(model, q, npoints, &besterror, bestfour);

    printf("error = %d\n",besterror);

    if (besterror < 10) mine = 0;
    else mine = 1;

    real = getreal(argv[m],argv[2]);

    confusion[mine][real]+=1;

    free(image);
  }

  printf("%d ",confusion[0][0]);
  printf("%d\n",confusion[0][1]);
  printf("%d ",confusion[1][0]);
  printf("%d\n",confusion[1][1]);

  printf("Exiting\n");
}


