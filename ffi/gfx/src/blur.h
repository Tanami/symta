/*

The MIT License (MIT)

Copyright (c) 2015 Michal Dymel, Nikita Sadkov

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*/


//FIXME: code should check for out of bounds error
//       otherwise larger r will likely produce segfaults

static void boxBlurH_4(int *source, int *dest, int w, int h, int r)
{
  int i, j;
  double iar = (double)1 / (r + r + 1);
  for (i=0; i<h; i++)
  { 
    int ti = i * w;
    int li = ti;
    int ri = ti + r;
    double fv = source[ti];
    double lv = source[ti + w - 1];
    double val = (r + 1) * fv;
    for (j = 0; j < r; j++) val += source[ti + j];
    for (j = 0; j <= r; j++)
    {
      val += source[ri++] - fv;
      dest[ti++] = (int)round(val * iar);
    }
    for (j = r + 1; j < w - r; j++)
    {
      val += source[ri++] - dest[li++];
      dest[ti++] = (int)round(val * iar);
    }
    for (j = w - r; j < w; j++)
    {
      val += lv - source[li++];
      dest[ti++] = (int)round(val * iar);
    }
  }
}

static void boxBlurT_4(int *source, int *dest, int w, int h, int r)
{
  int i, j;
  double iar = (double)1 / (r + r + 1);
  for (i=0; i<w; i++)
  {
    int ti = i;
    int li = ti;
    int ri = ti + r * w;
    double fv = source[ti];
    double lv = source[ti + w * (h - 1)];
    double val = (r + 1) * fv;
    for (j = 0; j < r; j++) val += source[ti + j * w];
    for (j = 0; j <= r; j++)
    {
      val += source[ri] - fv;
      dest[ti] = (int)round(val * iar);
      ri += w;
      ti += w;
    }
    for (j = r + 1; j < h - r; j++)
    {
      val += source[ri] - source[li];
      dest[ti] = (int)round(val * iar);
      li += w;
      ri += w;
      ti += w;
    }
    for (j = h - r; j < h; j++)
    {
      val += lv - source[li];
      dest[ti] = (int)round(val * iar);
      li += w;
      ti += w;
    }
  }
}

static void boxesForGauss(int sigma, int *sizes, int n)
{
  int i;
  int wl = (int)floor(sqrt(((double)12 * sigma * sigma / n) + 1));
  if (wl % 2 == 0) wl--;
  int wu = wl + 2;

  double mIdeal = ((double)12 * sigma * sigma - n * wl * wl - 4 * n * wl - 3 * n)
               / (-4 * wl - 4);
  int m = (int)round(mIdeal);

  for (i = 0; i < n; i++) sizes[i] = (int)(i < m ? wl : wu);
}

static void boxBlur_4(int *source, int *dest, int w, int h, int r)
{
  int i;
  for (i = 0; i < w*h; i++) dest[i] = source[i];
  boxBlurH_4(dest, source, w, h, r);
  boxBlurT_4(source, dest, w, h, r);
}

static void gaussBlur_4(int *source, int *dest, int w, int h, int r)
{
  int bxs[3];
  boxesForGauss(r, bxs, 3);
  boxBlur_4(source, dest, w, h, (bxs[0] - 1) / 2);
  boxBlur_4(dest, source, w, h, (bxs[1] - 1) / 2);
  boxBlur_4(source, dest, w, h, (bxs[2] - 1) / 2);
}

static void gaussian_blur(int *dst, int *src, int width, int height, int radial)
{
  int i;
  gaussBlur_4(src, dst, width, height, radial);
  for (i = 0; i < width*height; i++) {
    if (dst[i] < 0) dst[i] = 0;
    if (dst[i] > 255) dst[i] = 255;
  }
}
