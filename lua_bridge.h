#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "dispatch.h"

#include <car.h>

// Initialize lua and read in "drive.lua". Should be called if necessary
// from the robot's entry point, but you can cache the result in between
// calls to lua_sample.cpp's `drive` function
lua_State *initLua();

// Stop lua. Should be called from the robot's shutdown function
void closeLua(lua_State *L);

// Main drive function
void tl_drive(lua_State *L, tCarElt *car);
