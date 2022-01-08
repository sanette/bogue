/* Draw filled equilateral triangles using the SDL2 Renderer API */
/* of course it's much easier to use OPENGL directly! */

/* WARNING: no error test */

/* San Vu Ngoc, 2017 */
/* use at your own risk */

#include <SDL2/SDL.h>
#include <math.h>
#include <time.h>

struct color {
  Uint8 r;
  Uint8 g;
  Uint8 b;
  Uint8 a;
};

double rad(double angle)
{
  double pi = 4 * atan(1);
  return pi * angle / 180;
}

/* this will draw a filled equilateral triangle of given side, angle and */
/* color. One vertex of the triangle is at the center of the texture. */
/* Another vertex is at the "angle" direction. */
SDL_Texture* triangle(SDL_Renderer* renderer, int side, double angle, struct color c)
{
  Uint32 format = SDL_PIXELFORMAT_ARGB8888;
  int access = SDL_TEXTUREACCESS_TARGET;
  SDL_RendererFlip flip = SDL_FLIP_NONE;
  
  /* size of the rectangle containing the horizontal triangle */
  int w = side;
  int h = side * sqrt(3)/2;

  /* create a mask texture for cutting operations  */
  SDL_Texture* mask = SDL_CreateTexture(renderer, format, access, w, h);
  SDL_SetTextureBlendMode(mask, SDL_BLENDMODE_NONE);

  /* create a large target texture that will contain the rotated triangle with */
  /* one vertex at the center of the texture. The size could be made smaller */
  /* depending on angle, of course. */
  SDL_Texture* target = SDL_CreateTexture(renderer, format, access, 2*side, 2*side);
  SDL_SetTextureBlendMode(target, SDL_BLENDMODE_BLEND);

  /* draw rotated rectangle that will contain the triangle in 3 steps: */
  /* 1. fill the mask with color */
  SDL_SetRenderTarget(renderer, mask);
  SDL_SetRenderDrawColor(renderer, c.r, c.g, c.b, c.a);
  SDL_RenderClear(renderer);

  /* 2. clear the texture with 0 */
  SDL_SetRenderTarget(renderer, target);
  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
  SDL_RenderClear(renderer);

  /* 3. rotate and copy the mask onto the target texture */
  SDL_Point center = {0, 0};
  SDL_Rect dst = { side, side, w, h };
  SDL_RenderCopyEx(renderer, mask, NULL, &dst, angle, &center, flip);

  /* clear the mask with 0 */
  SDL_SetRenderTarget(renderer, mask);
  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
  SDL_RenderClear(renderer);

  /* cut top-left corner */
  SDL_SetRenderTarget(renderer, target);
  SDL_RenderCopyEx(renderer, mask, NULL, &dst, angle + 60, &center, flip);

  /* cut the top-right corner */
  int x = side * cos(rad(angle));
  int y = side * sin(rad(angle));
  SDL_Rect dst2 = { side+x-w, side+y, w, h };
  SDL_Point center2 = {w, 0};
  SDL_RenderCopyEx(renderer, mask, NULL, &dst2, angle - 60, &center2, flip);

  /* we don't need the mask anymore */
  SDL_DestroyTexture(mask);
  
  /* reset rendering target to default */
  SDL_SetRenderTarget(renderer, NULL);

  return target;
}


/* test */
int main()
{
  int w = 800;
  int h = 800;
  int max_side = (((w)<(h))?(w):(h)) / 5;
  
  SDL_Init(SDL_INIT_VIDEO);
  SDL_Window* window = SDL_CreateWindow("Triangles",
        SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, w, h, 0);
  SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_TARGETTEXTURE
);

  /* we draw random triangles */
  int i, j,  x, y, tw, th, side;
  double angle;
  srand(time(NULL));
 
  struct color c;
  SDL_Texture* trg;
  SDL_Rect dst;

  for( j = 0; j < 100; j = j + 1 ){
    SDL_RenderClear(renderer);
    
    for( i = 0; i < 50; i = i + 1 ){
      x = rand() % w;
      y = rand() % h;
      side = rand() % max_side + 1;
      angle = rand() % 360;
      c.r = rand() % 255;
      c.g = rand() % 255;
      c.b = rand() % 255;
      c.a = rand() % 255;
      trg = triangle(renderer, side, angle, c);
      SDL_QueryTexture(trg, NULL, NULL, &tw, &th);
      dst.x = x;
      dst.y = y;
      dst.w = tw;
      dst.h = th;
      SDL_RenderCopy(renderer, trg, NULL, &dst);
      SDL_DestroyTexture(trg);
    }
    
    SDL_RenderPresent(renderer);
    SDL_Delay(16);
  }
  
  SDL_Delay(5000);
  SDL_DestroyWindow(window);
  SDL_Quit ();
}


/*
gcc triangle.c -lSDL2 -lm -o triangle_c
*/
