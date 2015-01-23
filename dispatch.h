#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <car.h>
#include <robottools.h>

/** top-level functions **/

int tl_RtTrackSideTgAngleL(lua_State *L);

const luaL_Reg tl_functions[] = {
  {"RtTrackSideTgAngleL", tl_RtTrackSideTgAngleL},
  {NULL, NULL}
};

/** userdata definitions **/
typedef struct {
  tCarElt *wrapped;
} tl_CarElt;

typedef struct {
  tInitCar *wrapped;
} tl_InitCar;

typedef struct {
  tPublicCar *wrapped;
} tl_PublicCar;

/*
typedef struct {
  tCarRaceInfo *wrapped;
} tl_CarRaceInfo;
*/

typedef struct {
  tPrivCar *wrapped;
} tl_PrivCar;

/*
typedef struct {
  tCarPitCmd *wrapped;
} tl_CarPitCmd;
*/

typedef struct {
  tTrkLocPos *wrapped;
} tl_TrkLocPos;

typedef struct {
  tTrackSeg *wrapped;
} tl_TrackSeg;

typedef struct {
  tPosd *wrapped;
} tl_Posd;

typedef struct {
  tDynPt *wrapped;
} tl_DynPt;


/** Getters for tCarElt **/
int dispatch_CarElt(lua_State *L);

int f_CarElt_index(lua_State *L, tCarElt *car);
int f_CarElt_info(lua_State *L, tCarElt *car);
int f_CarElt_priv(lua_State *L, tCarElt *car);
int f_CarElt_pub(lua_State *L, tCarElt *car);
//int f_CarElt_race(lua_State *L, tCarElt *car);
//int f_CarElt_ctrl(lua_State *L, tCarElt *car);
//int f_CarElt_pitcmd(lua_State *L, tCarElt *car);

/** Getters for tInitCar **/
int dispatch_InitCar(lua_State *L);

int f_InitCar_name(lua_State *L, tInitCar *info);
int f_InitCar_teamname(lua_State *L, tInitCar *info);
int f_InitCar_carName(lua_State *L, tInitCar *info);
int f_InitCar_category(lua_State *L, tInitCar *info);
int f_InitCar_raceNumber(lua_State *L, tInitCar *info);
int f_InitCar_startRank(lua_State *L, tInitCar *info);
int f_InitCar_driverType(lua_State *L, tInitCar *info);
int f_InitCar_skillLevel(lua_State *L, tInitCar *info);
//int f_InitCar_iconColor(lua_State *L, tInitCar *info);
//int f_InitCar_dimension(lua_State *L, tInitCar *info);
//int f_InitCar_drvPos(lua_State *L, tInitCar *info);
//int f_InitCar_bonnetPos(lua_State *L, tInitCar *info);
int f_InitCar_tank(lua_State *L, tInitCar *info);
int f_InitCar_steerLock(lua_State *L, tInitCar *info);
//int f_InitCar_statGC(lua_State *L, tInitCar *info);
//int f_InitCar_wheel(lua_State *L, tInitCar *info);
//int f_InitCar_visualAttr(lua_State *L, tInitCar *info);

/** Getters for tPrivCar **/
int dispatch_PrivCar(lua_State *L);

int f_PrivCar_driverIndex(lua_State *L, tPrivCar *priv);
int f_PrivCar_modName(lua_State *L, tPrivCar *priv);
//int f_PrivCar_wheel(lua_State *L, tPrivCar *priv);
//int f_PrivCar_corner(lua_State *L, tPrivCar *priv);
int f_PrivCar_gear(lua_State *L, tPrivCar *priv);
int f_PrivCar_fuel(lua_State *L, tPrivCar *priv);
int f_PrivCar_enginerpm(lua_State *L, tPrivCar *priv);
int f_PrivCar_enginerpmRedLine(lua_State *L, tPrivCar *priv);
int f_PrivCar_enginerpmMax(lua_State *L, tPrivCar *priv);
int f_PrivCar_enginerpmMaxTq(lua_State *L, tPrivCar *priv);
int f_PrivCar_enginerpmMaxPw(lua_State *L, tPrivCar *priv);
int f_PrivCar_engineMaxTq(lua_State *L, tPrivCar *priv);
int f_PrivCar_engineMaxPw(lua_State *L, tPrivCar *priv);
//int f_PrivCar_gearRatio(lua_State *L, tPrivCar *priv);
int f_PrivCar_gearNb(lua_State *L, tPrivCar *priv);
int f_PrivCar_gearOffset(lua_State *L, tPrivCar *priv);
//int f_PrivCar_skid(lua_State *L, tPrivCar *priv);
//int f_PrivCar_reaction(lua_State *L, tPrivCar *priv);
int f_PrivCar_collision(lua_State *L, tPrivCar *priv);
int f_PrivCar_simcollision(lua_State *L, tPrivCar *priv);
int f_PrivCar_smoke(lua_State *L, tPrivCar *priv);
//int f_PrivCar_normal(lua_State *L, tPrivCar *priv);
//int f_PrivCar_collpos(lua_State *L, tPrivCar *priv);
int f_PrivCar_dammage(lua_State *L, tPrivCar *priv);
int f_PrivCar_debug(lua_State *L, tPrivCar *priv);
//int f_PrivCar_collision_state(lua_State *L, tPrivCar *priv);

/** Getters for tPublicCar **/
int dispatch_PublicCar(lua_State *L);

int f_PublicCar_DynGC(lua_State *L, tPublicCar *pub);
int f_PublicCar_DynGCg(lua_State *L, tPublicCar *pub);
int f_PublicCar_speed(lua_State *L, tPublicCar *pub);
//int f_PublicCar_posMat(lua_State *L, tPublicCar *pub);
int f_PublicCar_trkPos(lua_State *L, tPublicCar *pub);
int f_PublicCar_state(lua_State *L, tPublicCar *pub);
//int f_PublicCar_corner(lua_State *L, tPublicCar *pub);

/** Getters for tTrkLocPos **/
int dispatch_TrkLocPos(lua_State *L);

int f_TrkLocPos_seg(lua_State *L, tTrkLocPos *pos);
int f_TrkLocPos_type(lua_State *L, tTrkLocPos *pos);
int f_TrkLocPos_toStart(lua_State *L, tTrkLocPos *pos);
int f_TrkLocPos_toRight(lua_State *L, tTrkLocPos *pos);
int f_TrkLocPos_toMiddle(lua_State *L, tTrkLocPos *pos);
int f_TrkLocPos_toLeft(lua_State *L, tTrkLocPos *pos);

/** Getters for tTrackSeg **/
int dispatch_TrackSeg(lua_State *L);

int f_TrackSeg_width(lua_State *L, tTrackSeg *seg);

/** Getters for tPosd **/
int dispatch_Posd(lua_State *L);

int f_Posd_x(lua_State *L, tPosd *pos);
int f_Posd_y(lua_State *L, tPosd *pos);
int f_Posd_z(lua_State *L, tPosd *pos);
int f_Posd_ax(lua_State *L, tPosd *pos);
int f_Posd_ay(lua_State *L, tPosd *pos);
int f_Posd_az(lua_State *L, tPosd *pos);

/** Getters for tDynPt **/
int dispatch_DynPt(lua_State *L);

int f_DynPt_pos(lua_State *L, tDynPt *pt);
int f_DynPt_vel(lua_State *L, tDynPt *pt);
int f_DynPt_acc(lua_State *L, tDynPt *pt);

/** Struct dispatch functions **/
const luaL_Reg dispatchers[] = {
  {"torcs.CarElt", dispatch_CarElt},
  {"torcs.InitCar", dispatch_InitCar},
  {"torcs.PrivCar", dispatch_PrivCar},
  {"torcs.PublicCar", dispatch_PublicCar},
  {"torcs.TrkLocPos", dispatch_TrkLocPos},
  {"torcs.TrackSeg", dispatch_TrackSeg},
  {"torcs.Posd", dispatch_Posd},
  {"torcs.DynPt", dispatch_DynPt},
  {NULL, NULL}
};

/** Field dispatch tables **/
typedef int (*getter_CarElt) (lua_State *L, tCarElt *car);

typedef struct {
  const char *name;
  getter_CarElt getter;
} getterEntry_CarElt;

const getterEntry_CarElt fields_CarElt[] = {
  {"index", f_CarElt_index},
  {"info", f_CarElt_info},
  {"pub", f_CarElt_pub},
  //  {"race", f_CarElt_race},
  {"priv", f_CarElt_priv},
  //  {"ctrl", f_CarElt_ctrl},
  //  {"pitcmd", f_CarElt_pitcmd},
  {NULL, NULL}
};


typedef int (*getter_InitCar) (lua_State *L, tInitCar *info);

typedef struct {
  const char *name;
  getter_InitCar getter;
} getterEntry_InitCar;

const getterEntry_InitCar fields_InitCar[] = {
  {"name", f_InitCar_name},
  {"teamname", f_InitCar_teamname},
  {"carName", f_InitCar_carName},
  {"category", f_InitCar_category},
  {"raceNumber", f_InitCar_raceNumber},
  {"startRank", f_InitCar_startRank},
  {"driverType", f_InitCar_driverType},
  {"skillLevel", f_InitCar_skillLevel},
  //  {"iconColor", f_InitCar_iconColor},
  //  {"dimension", f_InitCar_dimension},
  //  {"drvPos", f_InitCar_drvPos},
  //  {"bonnetPos", f_InitCar_bonnetPos},
  {"tank", f_InitCar_tank},
  {"steerLock", f_InitCar_steerLock},
  //  {"statGC", f_InitCar_statGC},
  //  {"wheel", f_InitCar_wheel},
  //  {"visualAttr", f_InitCar_visualAttr},
  {NULL, NULL}
};


typedef int (*getter_PrivCar) (lua_State *L, tPrivCar *priv);

typedef struct {
  const char *name;
  getter_PrivCar getter;
} getterEntry_PrivCar;

const getterEntry_PrivCar fields_PrivCar[] = {
  {"driverIndex", f_PrivCar_driverIndex},
  {"modName", f_PrivCar_modName},
  //  {"wheel", f_PrivCar_wheel},
  //  {"corner", f_PrivCar_corner},
  {"gear", f_PrivCar_gear},
  {"fuel", f_PrivCar_fuel},
  {"enginerpm", f_PrivCar_enginerpm},
  {"enginerpmRedLine", f_PrivCar_enginerpmRedLine},
  {"enginerpmMax", f_PrivCar_enginerpmMax},
  {"enginerpmMaxTq", f_PrivCar_enginerpmMaxTq},
  {"enginerpmMaxPw", f_PrivCar_enginerpmMaxPw},
  {"engineMaxTq", f_PrivCar_engineMaxTq},
  {"engineMaxPw", f_PrivCar_engineMaxPw},
  //  {"gearRatio", f_PrivCar_gearRatio},
  {"gearNb", f_PrivCar_gearNb},
  {"gearOffset", f_PrivCar_gearOffset},
  //  {"skid", f_PrivCar_skid},
  //  {"reaction", f_PrivCar_reaction},
  {"collision", f_PrivCar_collision},
  {"simcollision", f_PrivCar_simcollision},
  {"smoke", f_PrivCar_smoke},
  //  {"normal", f_PrivCar_normal},
  //  {"collpos", f_PrivCar_collpos},
  {"dammage", f_PrivCar_dammage},
  {"debug", f_PrivCar_debug},
  //  {"collision", f_PrivCar_collision_state},
  {NULL, NULL}
};


typedef int (*getter_PublicCar) (lua_State *L, tPublicCar *pub);

typedef struct {
  const char *name;
  getter_PublicCar getter;
} getterEntry_PublicCar;

const getterEntry_PublicCar fields_PublicCar[] = {
  {"DynGC", f_PublicCar_DynGC},
  {"DynGCg", f_PublicCar_DynGCg},
  {"speed", f_PublicCar_speed},
  //  {"posMat", f_PublicCar_posMat},
  {"trkPos", f_PublicCar_trkPos},
  {"state", f_PublicCar_state},
  //  {"corner", f_PublicCar_corner},
  {NULL, NULL}
};


typedef int (*getter_TrkLocPos) (lua_State *L, tTrkLocPos *pos);

typedef struct {
  const char *name;
  getter_TrkLocPos getter;
} getterEntry_TrkLocPos;

const getterEntry_TrkLocPos fields_TrkLocPos[] = {
  {"seg", f_TrkLocPos_seg},
  {"type", f_TrkLocPos_type},
  {"toStart", f_TrkLocPos_toStart},
  {"toRight", f_TrkLocPos_toRight},
  {"toMiddle", f_TrkLocPos_toMiddle},
  {"toLeft", f_TrkLocPos_toLeft},
  {NULL, NULL}
};


typedef int (*getter_TrackSeg) (lua_State *L, tTrackSeg *seg);

typedef struct {
  const char *name;
  getter_TrackSeg getter;
} getterEntry_TrackSeg;

const getterEntry_TrackSeg fields_TrackSeg[] = {
  {"width", f_TrackSeg_width},
  {NULL, NULL}
};


typedef int (*getter_Posd) (lua_State *L, tPosd *pos);

typedef struct {
  const char *name;
  getter_Posd getter;
} getterEntry_Posd;

const getterEntry_Posd fields_Posd[] = {
  {"x", f_Posd_x},
  {"y", f_Posd_y},
  {"z", f_Posd_z},
  {"ax", f_Posd_ax},
  {"ay", f_Posd_ay},
  {"az", f_Posd_az},
  {NULL, NULL}
};


typedef int (*getter_DynPt) (lua_State *L, tDynPt *pt);

typedef struct {
  const char *name;
  getter_DynPt getter;
} getterEntry_DynPt;

const getterEntry_DynPt fields_DynPt[] = {
  {"pos", f_DynPt_pos},
  {"vel", f_DynPt_vel},
  {"acc", f_DynPt_acc},
  {NULL, NULL}
};
