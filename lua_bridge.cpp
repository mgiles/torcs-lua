#include "lua_bridge.h"

static tdble getNumberField(lua_State *L, const char *field, tdble def);
static int getIntField(lua_State *L, const char *field, int def);

void initTorcs(lua_State *L) {
  // Set up top-level functions
  luaL_register(L, "torcs", tl_functions);
  lua_pop(L, 1); // "torcs" table registered, don't need it

  // Set up the field access metatables
  int i = 0;
  while (dispatchers[i].name != NULL) {
    luaL_newmetatable(L, dispatchers[i].name);
    lua_pushstring(L, "__index");
    lua_pushcfunction(L, dispatchers[i].func);
    lua_settable(L, -3);
    lua_pop(L, 1); // Metatable already in registry

    i++;
  }
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

  // Wrap car in a tl_CarElt value
  tl_CarElt *wrapper = (tl_CarElt *) lua_newuserdata(L, sizeof(tl_CarElt));
  wrapper->wrapped = car;

  // Set its field access metatable (defined in initTorcs)
  luaL_getmetatable(L, "torcs.CarElt");
  lua_setmetatable(L, -2);

  // Call drive
  int res = lua_pcall(L, 1, 1, 0);
  if (res != 0) {
    const char *error = lua_tostring(L, -1);
    printf("Error in Lua code: %s\n", error);
    return;
  }

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
