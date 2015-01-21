#include "dispatch.h"

tCarElt *checkCar(lua_State *L);

const struct luaL_Reg torcs_functions[] = {
  {NULL, NULL}
};

int tl_newTorcsState(lua_State *L) {
  tCarElt *car = (tCarElt *) lua_touserdata(L, 1);
  tTorcsState *state = (tTorcsState *) lua_newuserdata(L, sizeof(tTorcsState));
  state->car = car;

  // Hook up dispatching code to the `state` userdata
  luaL_getmetatable(L, "torcs.state");
  lua_setmetatable(L, -2);
  return 1;
}

int tl_lookupField(lua_State *L) {
  tCarElt *car = checkCar(L);
  const char *field = luaL_checkstring(L, 2);

  tFieldGetter f = NULL;
  int i = 0;
  while (carFields[i].name != NULL && strcmp(carFields[i].name, field) != 0) {
    i++;
  }
  f = carFields[i].getter;

  if (f != NULL) {
    return f(L, car);
  } else {
    char error[strlen(field) + 30];
    sprintf(error, "No such field: %s", field);
    lua_pushstring(L, error);
    lua_error(L);

    return -1; // Never reached, lua_error doesn't return
  }
}

tCarElt *checkCar(lua_State *L) {
  tTorcsState *state = (tTorcsState *) luaL_checkudata(L, 1, "torcs.state");
  luaL_argcheck(L, state != NULL, 1, "Expected torcsState");
  return state->car;
}

const struct tFieldEntry carFields[] = {
  // Hand-written
  {"steerLock", tl_steerLock},

  // Generated
  {"driverIndex", tl_driverIndex},
  {"gear", tl_gear},
  {"fuel", tl_fuel},
  {"enginerpm", tl_enginerpm},
  {"enginerpmRedLine", tl_enginerpmRedLine},
  {"enginerpmMax", tl_enginerpmMax},
  {"enginerpmMaxTq", tl_enginerpmMaxTq},
  {"enginerpmMaxPw", tl_enginerpmMaxPw},
  {"engineMaxTq", tl_engineMaxTq},
  {"engineMaxPw", tl_engineMaxPw},
  {"gearNb", tl_gearNb},
  {"gearOffset", tl_gearOffset},
  {"dammage", tl_dammage},
  {"debug", tl_debug},
  {NULL, NULL}
};

// Hand-written getters

int tl_steerLock(lua_State *L, tCarElt *car) {
  lua_pushnumber(L, car->_steerLock);
  return 1;
}

// Generated getters

int tl_driverIndex(lua_State *L, tCarElt *car) {
  lua_pushinteger(L, car->_driverIndex);
  return 1;
}

int tl_gear(lua_State *L, tCarElt *car) {
  lua_pushinteger(L, car->_gear);
  return 1;
}

int tl_fuel(lua_State *L, tCarElt *car) {
  lua_pushnumber(L, car->_fuel);
  return 1;
}

int tl_enginerpm(lua_State *L, tCarElt *car) {
  lua_pushnumber(L, car->_enginerpm);
  return 1;
}

int tl_enginerpmRedLine(lua_State *L, tCarElt *car) {
  lua_pushnumber(L, car->_enginerpmRedLine);
  return 1;
}

int tl_enginerpmMax(lua_State *L, tCarElt *car) {
  lua_pushnumber(L, car->_enginerpmMax);
  return 1;
}

int tl_enginerpmMaxTq(lua_State *L, tCarElt *car) {
  lua_pushnumber(L, car->_enginerpmMaxTq);
  return 1;
}

int tl_enginerpmMaxPw(lua_State *L, tCarElt *car) {
  lua_pushnumber(L, car->_enginerpmMaxPw);
  return 1;
}

int tl_engineMaxTq(lua_State *L, tCarElt *car) {
  lua_pushnumber(L, car->_engineMaxTq);
  return 1;
}

int tl_engineMaxPw(lua_State *L, tCarElt *car) {
  lua_pushnumber(L, car->_engineMaxPw);
  return 1;
}

int tl_gearNb(lua_State *L, tCarElt *car) {
  lua_pushinteger(L, car->_gearNb);
  return 1;
}

int tl_gearOffset(lua_State *L, tCarElt *car) {
  lua_pushinteger(L, car->_gearOffset);
  return 1;
}

int tl_dammage(lua_State *L, tCarElt *car) {
  lua_pushinteger(L, car->_dammage);
  return 1;
}

int tl_debug(lua_State *L, tCarElt *car) {
  lua_pushinteger(L, car->_debug);
  return 1;
}
