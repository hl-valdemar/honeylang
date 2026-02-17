#define MAX_POINTS 100

struct vec2 {
    float x;
    float y;
};

struct rect {
    struct vec2 origin;
    struct vec2 size;
};

int add(int a, int b);
