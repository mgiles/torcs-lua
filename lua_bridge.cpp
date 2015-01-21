#include "lua_bridge.h"

static tdble getNumberField(lua_State *L, const char *field, tdble def);
static int getIntField(lua_State *L, const char *field, int def);

void initTorcs(lua_State *L) {
  // Register the "torcs" functions for Lua
  luaL_register(L, "torcs", torcs_functions);

  // Set the dispatch function for field access
  // on torcsState userdata
  luaL_newmetatable(L, "torcs.state");
  lua_pushstring(L, "__index");
  lua_pushcfunction(L, tl_lookupField);
  lua_settable(L, -3);

  lua_pop(L, 1); // Metatable already in registry from newmetatable
}

lua_State *initLua() {
  lua_State *L = lua_open();
  luaL_openlibs(L);

  initTorcs(L);

  const char *torcsBase = getenv("TORCS_BASE");
  const char *rest = "/src/drivers/lua_sample/lua/drive.lua";
  char path[strlen(torcsBase) + strlen(rest) + 1];
  sprintf(path, "%s%s", torcsBase, rest);

  if (luaL_dofile(L, path)) {
    printf("error reading drive.lua\n");
    printf("Error:\n");
    printf("%s", lua_tostring(L, -1));
    lua_pop(L, 1);
  }

  return L;
}

void closeLua(lua_State *L) {
  lua_close(L);
}

void tl_drive(lua_State *L, tCarElt *car) {
  lua_getglobal(L, "drive");

  // Wrap car in a torcsState value
  lua_pushcfunction(L, tl_newTorcsState);
  lua_pushlightuserdata(L, car);
  lua_call(L, 1, 1);

  // Call drive
  lua_call(L, 1, 1);

  car->ctrl.steer = getNumberField(L, "steer", 0.0);
  car->ctrl.accelCmd = getNumberField(L, "accelCmd", 0.0);
  car->ctrl.brakeCmd = getNumberField(L, "brakeCmd", 0.0);
  car->ctrl.clutchCmd = getNumberField(L, "clutchCmd", 0.0);
  car->ctrl.gear = getIntField(L, "gear", 0);
  car->ctrl.raceCmd = getIntField(L, "raceCmd", 0);
  car->ctrl.lightCmd = getIntField(L, "lightCmd", 0);
  // msg, msgColor omitted
}

static tdble getNumberField(lua_State *L, const char *field, tdble def) {
  tdble value;

  lua_getfield(L, -1, field);
  if (lua_isnumber(L, -1)) {
    value = lua_tonumber(L, -1);
  } else {
    value = def;
  }

  lua_pop(L, 1);
  return value;
}

static int getIntField(lua_State *L, const char *field, int def) {
  int value;

  lua_getfield(L, -1, field);
  if (lua_isnumber(L, -1)) {
    value = lua_tointeger(L, -1);
  } else {
    value = def;
  }

  lua_pop(L, 1);
  return value;
}
