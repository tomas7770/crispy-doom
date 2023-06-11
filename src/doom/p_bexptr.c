//
// Copyright(C) 1993-1996 Id Software, Inc.
// Copyright(C) 1999 id Software, Chi Hoang, Lee Killough, Jim Flynn, Rand Phares, Ty Halderman
// Copyright(C) 2005-2014 Simon Howard
// Copyright(C) 2017 Fabian Greffrath
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// DESCRIPTION:
//	[crispy] additional BOOM and MBF code pointers
//	[custom] MBF21 code pointers
//

#include "p_local.h"
#include "m_random.h"
#include "s_sound.h"
#include "d_event.h"

extern void A_Explode();
extern void A_FaceTarget();
extern void P_BulletSlope();
extern boolean P_CheckAmmo();
extern void P_SetPsprite (player_t *player, int position, statenum_t stnum);
extern void P_RadiusAttackCustomDist();

extern boolean P_CheckMeleeRange (mobj_t *actor);
extern void P_Thrust (player_t* player, angle_t angle, fixed_t move);

// killough 11/98: kill an object
void A_Die(mobj_t *actor)
{
  P_DamageMobj(actor, NULL, NULL, actor->health);
}

//
// A_Detonate
// killough 8/9/98: same as A_Explode, except that the damage is variable
//

void A_Detonate(mobj_t *mo)
{
  P_RadiusAttack(mo, mo->target, mo->info->damage);
}

//
// killough 9/98: a mushroom explosion effect, sorta :)
// Original idea: Linguica
//

void A_Mushroom(mobj_t *actor)
{
  int i, j, n = actor->info->damage;

  // Mushroom parameters are part of code pointer's state
  fixed_t misc1 = actor->state->misc1 ? actor->state->misc1 : FRACUNIT*4;
  fixed_t misc2 = actor->state->misc2 ? actor->state->misc2 : FRACUNIT/2;

  A_Explode(actor);               // make normal explosion

  for (i = -n; i <= n; i += 8)    // launch mushroom cloud
    for (j = -n; j <= n; j += 8)
      {
	mobj_t target = *actor, *mo;
	target.x += i << FRACBITS;    // Aim in many directions from source
	target.y += j << FRACBITS;
	target.z += P_AproxDistance(i,j) * misc1;           // Aim fairly high
	mo = P_SpawnMissile(actor, &target, MT_FATSHOT);    // Launch fireball
	mo->momx = FixedMul(mo->momx, misc2);
	mo->momy = FixedMul(mo->momy, misc2);               // Slow down a bit
	mo->momz = FixedMul(mo->momz, misc2);
	mo->flags &= ~MF_NOGRAVITY;   // Make debris fall under gravity
      }
}

//
// A_BetaSkullAttack()
// killough 10/98: this emulates the beta version's lost soul attacks
//

void A_BetaSkullAttack(mobj_t *actor)
{
  int damage;
  if (!actor->target || actor->target->type == MT_SKULL)
    return;
  S_StartSound(actor, actor->info->attacksound);
  A_FaceTarget(actor);
  damage = (P_Random(/* pr_skullfly */)%8+1)*actor->info->damage;
  P_DamageMobj(actor->target, actor, actor, damage);
}

void A_Stop(mobj_t *actor)
{
  actor->momx = actor->momy = actor->momz = 0;
}

//
// killough 11/98
//
// The following were inspired by Len Pitre
//
// A small set of highly-sought-after code pointers
//

void A_Spawn(mobj_t *mo)
{
  if (mo->state->misc1)
    {
/*    mobj_t *newmobj = */ P_SpawnMobj(mo->x, mo->y,
				    (mo->state->misc2 << FRACBITS) + mo->z,
				    mo->state->misc1 - 1);
//    newmobj->flags = (newmobj->flags & ~MF_FRIEND) | (mo->flags & MF_FRIEND);

    }
}

void A_Turn(mobj_t *mo)
{
  mo->angle += (angle_t)(((uint64_t) mo->state->misc1 << 32) / 360);
}

void A_Face(mobj_t *mo)
{
  mo->angle = (angle_t)(((uint64_t) mo->state->misc1 << 32) / 360);
}

void A_Scratch(mobj_t *mo)
{
  mo->target && (A_FaceTarget(mo), P_CheckMeleeRange(mo)) ?
    mo->state->misc2 ? S_StartSound(mo, mo->state->misc2) : (void) 0,
    P_DamageMobj(mo->target, mo, mo, mo->state->misc1) : (void) 0;
}

void A_PlaySound(mobj_t *mo)
{
  S_StartSound(mo->state->misc2 ? NULL : mo, mo->state->misc1);
}

// [crispy] this is pretty much the only action pointer that makes sense for both mobj and pspr states
void A_RandomJump(mobj_t *mo, player_t *player, pspdef_t *psp)
{
	// [crispy] first, try to apply to pspr states
	if (player && psp)
	{
		if (Crispy_Random() < psp->state->misc2)
		{
			// extern void P_SetPsprite (player_t *player, int position, statenum_t stnum);

			P_SetPsprite(player, psp - &player->psprites[0], psp->state->misc1);
		}
	}
	else
	// [crispy] second, apply to mobj states
	if (mo)
	{
		if (Crispy_Random() < mo->state->misc2)
		{
			P_SetMobjState(mo, mo->state->misc1);
		}
	}
}

//
// This allows linedef effects to be activated inside deh frames.
//

void A_LineEffect(mobj_t *mo)
{
//if (!(mo->intflags & MIF_LINEDONE))                // Unless already used up
    {
      line_t junk = *lines;                          // Fake linedef set to 1st
      if ((junk.special = (short)mo->state->misc1))  // Linedef type
	{
	  static player_t player;                    // [crispy] made static
	  player_t *oldplayer = mo->player;          // Remember player status
	  mo->player = &player;                      // Fake player
	  player.health = 100;                       // Alive player
	  junk.tag = (short)mo->state->misc2;        // Sector tag for linedef
	  if (!P_UseSpecialLine(mo, &junk, 0))       // Try using it
	    P_CrossSpecialLinePtr(&junk, 0, mo);     // Try crossing it
//	  if (!junk.special)                         // If type cleared,
//	    mo->intflags |= MIF_LINEDONE;            // no more for this thing
	  mo->player = oldplayer;                    // Restore player status
	}
    }
}

//
// A_FireOldBFG
//
// This function emulates Doom's Pre-Beta BFG
// By Lee Killough 6/6/98, 7/11/98, 7/19/98, 8/20/98
//
// This code may not be used in other mods without appropriate credit given.
// Code leeches will be telefragged.

void A_FireOldBFG(mobj_t *mobj, player_t *player, pspdef_t *psp)
{
  int type = MT_PLASMA1;
  extern void P_CheckMissileSpawn (mobj_t* th);

  if (!player) return; // [crispy] let pspr action pointers get called from mobj states

  player->ammo[weaponinfo[player->readyweapon].ammo]--;

  player->extralight = 2;

  do
    {
      mobj_t *th, *mo = player->mo;
      angle_t an = mo->angle;
      angle_t an1 = ((P_Random(/* pr_bfg */)&127) - 64) * (ANG90/768) + an;
      angle_t an2 = ((P_Random(/* pr_bfg */)&127) - 64) * (ANG90/640) + ANG90;
//    extern int autoaim;

//    if (autoaim || !beta_emulation)
	{
	  // killough 8/2/98: make autoaiming prefer enemies
	  int mask = 0;//MF_FRIEND;
	  fixed_t slope;
	  if (critical->freeaim == FREEAIM_DIRECT)
	    slope = PLAYER_SLOPE(player);
	  else
	  do
	    {
	      slope = P_AimLineAttack(mo, an, 16*64*FRACUNIT);//, mask);
	      if (!linetarget)
		slope = P_AimLineAttack(mo, an += 1<<26, 16*64*FRACUNIT);//, mask);
	      if (!linetarget)
		slope = P_AimLineAttack(mo, an -= 2<<26, 16*64*FRACUNIT);//, mask);
	      if (!linetarget)
		slope = (critical->freeaim == FREEAIM_BOTH) ? PLAYER_SLOPE(player) : 0, an = mo->angle;
	    }
	  while (mask && (mask=0, !linetarget));     // killough 8/2/98
	  an1 += an - mo->angle;
	  // [crispy] consider negative slope
	  if (slope < 0)
	    an2 -= tantoangle[-slope >> DBITS];
	  else
	  an2 += tantoangle[slope >> DBITS];
	}

      th = P_SpawnMobj(mo->x, mo->y,
		       mo->z + 62*FRACUNIT - player->psprites[ps_weapon].sy,
		       type);
      // [NS] Play projectile sound.
      if (th->info->seesound)
      {
	S_StartSound (th, th->info->seesound);
      }
      th->target = mo; // P_SetTarget(&th->target, mo);
      th->angle = an1;
      // [NS] Use speed from thing info.
      th->momx = FixedMul(th->info->speed, finecosine[an1>>ANGLETOFINESHIFT]);
      th->momy = FixedMul(th->info->speed, finesine[an1>>ANGLETOFINESHIFT]);
      th->momz = FixedMul(th->info->speed, finetangent[an2>>ANGLETOFINESHIFT]);
      // [crispy] suppress interpolation of player missiles for the first tic
      th->interp = -1;
      P_CheckMissileSpawn(th);
    }
  while ((type != MT_PLASMA2) && (type = MT_PLASMA2)); //killough: obfuscated!
}

// [custom] MBF21 code pointers, adapted from dsda-doom

inline static fixed_t AngleToSlope(int a)
{
	if (a > ANG90)
		return finetangent[0];
	else if (-a > ANG90)
		return finetangent[FINEANGLES / 2 - 1];
	else
		return finetangent[(ANG90 - a) >> ANGLETOFINESHIFT];
}

inline static angle_t FixedToAngle(fixed_t a)
{
	return (angle_t)(((uint64_t)a * ANG1) >> FRACBITS);
}

// [XA] Ditto, using fixed-point-degrees input
inline static fixed_t DegToSlope(fixed_t a)
{
	if (a >= 0)
		return AngleToSlope(FixedToAngle(a));
	else
		return AngleToSlope(-(int)FixedToAngle(-a));
}

angle_t P_RandomHitscanAngle(fixed_t spread)
{
  int64_t spread_bam;

  // FixedToAngle doesn't work for negative numbers,
  // so for convenience take just the absolute value.
  spread_bam = (spread < 0 ? FixedToAngle(-spread) : FixedToAngle(spread));
  return (angle_t)((spread_bam * P_SubRandom()) / 255);
}

fixed_t P_RandomHitscanSlope(fixed_t spread)
{
  return AngleToSlope(P_RandomHitscanAngle(spread));
}

void A_SpawnObject(mobj_t *actor)
{
  int type, angle, ofs_x, ofs_y, ofs_z, vel_x, vel_y, vel_z;
  angle_t an;
  int fan, dx, dy;
  mobj_t *mo;

  if (!actor->state->args[0])
    return;

  type  = actor->state->args[0] - 1;
  angle = actor->state->args[1];
  ofs_x = actor->state->args[2];
  ofs_y = actor->state->args[3];
  ofs_z = actor->state->args[4];
  vel_x = actor->state->args[5];
  vel_y = actor->state->args[6];
  vel_z = actor->state->args[7];

  // calculate position offsets
  an = actor->angle + (unsigned int)(((int64_t)angle << 16) / 360);
  fan = an >> ANGLETOFINESHIFT;
  dx = FixedMul(ofs_x, finecosine[fan]) - FixedMul(ofs_y, finesine[fan]  );
  dy = FixedMul(ofs_x, finesine[fan]  ) + FixedMul(ofs_y, finecosine[fan]);

  // spawn it, yo
  mo = P_SpawnMobj(actor->x + dx, actor->y + dy, actor->z + ofs_z, type);
  if (!mo)
    return;

  // angle dangle
  mo->angle = an;

  // set velocity
  mo->momx = FixedMul(vel_x, finecosine[fan]) - FixedMul(vel_y, finesine[fan]  );
  mo->momy = FixedMul(vel_x, finesine[fan]  ) + FixedMul(vel_y, finecosine[fan]);
  mo->momz = vel_z;

  // if spawned object is a missile, set target+tracer
  if (mo->info->flags & (MF_MISSILE | MF_BOUNCES))
  {
    // if spawner is also a missile, copy 'em
    if (actor->info->flags & (MF_MISSILE | MF_BOUNCES))
    {
      mo->target = actor->target;
      mo->tracer = actor->tracer;
    }
    // otherwise, set 'em as if a monster fired 'em
    else
    {
      mo->target = actor;
      mo->tracer = actor->target;
    }
  }
}

void A_MonsterProjectile (mobj_t *actor)
{
    int type, angle, pitch, spawnofs_xy, spawnofs_z;
    mobj_t *mo;
    int an;

    if (!actor->target || !actor->state->args[0])
        return;

    type        = actor->state->args[0] - 1;
    angle       = actor->state->args[1];
    pitch       = actor->state->args[2];
    spawnofs_xy = actor->state->args[3];
    spawnofs_z  = actor->state->args[4];

    A_FaceTarget(actor);
    mo = P_SpawnMissile(actor, actor->target, type);
    if (!mo)
        return;

    // adjust angle
    mo->angle += (unsigned int)(((int64_t)angle << 16) / 360);
    an = mo->angle >> ANGLETOFINESHIFT;
    mo->momx = FixedMul(mo->info->speed, finecosine[an]);
    mo->momy = FixedMul(mo->info->speed, finesine[an]);

    // adjust pitch (approximated, using Doom's ye olde
    // finetangent table; same method as monster aim)
    mo->momz += FixedMul(mo->info->speed, DegToSlope(pitch));

    // adjust position
    an = (actor->angle - ANG90) >> ANGLETOFINESHIFT;
    mo->x += FixedMul(spawnofs_xy, finecosine[an]);
    mo->y += FixedMul(spawnofs_xy, finesine[an]);
    mo->z += spawnofs_z;

    // always set the 'tracer' field, so this pointer
    // can be used to fire seeker missiles at will.
    mo->tracer = actor->target;
}

void A_RadiusDamage(mobj_t *actor)
{
  if (!actor->state)
    return;

  P_RadiusAttackCustomDist(actor, actor->target, actor->state->args[0], actor->state->args[1]);
}

void A_WeaponProjectile(mobj_t *mobj, player_t *player, pspdef_t *psp)
{
  int type, angle, pitch, spawnofs_xy, spawnofs_z;
  mobj_t *mo;
  int an;

  if (!player) return; // [crispy] let pspr action pointers get called from mobj states

  if (!psp->state || !psp->state->args[0])
    return;

  type        = psp->state->args[0] - 1;
  angle       = psp->state->args[1];
  pitch       = psp->state->args[2];
  spawnofs_xy = psp->state->args[3];
  spawnofs_z  = psp->state->args[4];

  mo = P_SpawnPlayerMissile(player->mo, type);
  if (!mo)
    return;

  // adjust angle
  mo->angle += (unsigned int)(((int64_t)angle << 16) / 360);
  an = mo->angle >> ANGLETOFINESHIFT;
  mo->momx = FixedMul(mo->info->speed, finecosine[an]);
  mo->momy = FixedMul(mo->info->speed, finesine[an]);

  // adjust pitch (approximated, using Doom's ye olde
  // finetangent table; same method as autoaim)
  mo->momz += FixedMul(mo->info->speed, DegToSlope(pitch));

  // adjust position
  an = (player->mo->angle - ANG90) >> ANGLETOFINESHIFT;
  mo->x += FixedMul(spawnofs_xy, finecosine[an]);
  mo->y += FixedMul(spawnofs_xy, finesine[an]);
  mo->z += spawnofs_z;

  // set tracer to the player's autoaim target,
  // so player seeker missiles prioritizing the
  // baddie the player is actually aiming at. ;)
  mo->tracer = linetarget;
}

void A_WeaponBulletAttack(mobj_t *mobj, player_t *player, pspdef_t *psp)
{
  int hspread, vspread, numbullets, damagebase, damagemod;
  int i, damage, angle, slope;

  if (!player) return; // [crispy] let pspr action pointers get called from mobj states

  if (!psp->state)
		return;

  hspread    = psp->state->args[0];
  vspread    = psp->state->args[1];
  numbullets = psp->state->args[2];
  damagebase = psp->state->args[3];
  damagemod  = psp->state->args[4];

  P_BulletSlope(player->mo);

  for (i = 0; i < numbullets; i++)
  {
    damage = (P_Random() % damagemod + 1) * damagebase;
    angle = player->mo->angle + P_RandomHitscanAngle(hspread);
    slope = bulletslope + P_RandomHitscanSlope(vspread);

    P_LineAttack(player->mo, angle, MISSILERANGE, slope, damage);
  }
}

void A_WeaponMeleeAttack(mobj_t *mobj, player_t *player, pspdef_t *psp)
{
  int damagebase, damagemod, zerkfactor, hitsound, range;
  angle_t angle;
  int slope, damage;

  if (!player) return; // [crispy] let pspr action pointers get called from mobj states

  if (!psp->state)
    return;

  damagebase = psp->state->args[0];
  damagemod  = psp->state->args[1];
  zerkfactor = psp->state->args[2];
  hitsound   = psp->state->args[3];
  range      = psp->state->args[4];

  if (range == 0)
    range = MELEERANGE;

  damage = (P_Random() % damagemod + 1) * damagebase;
  if (player->powers[pw_strength])
    damage = (damage * zerkfactor) >> FRACBITS;

  // slight randomization; weird vanillaism here. :P
  angle = player->mo->angle;

  angle += P_SubRandom()<<18;

  // autoaim
  slope = P_AimLineAttack(player->mo, angle, range);

  // attack, dammit!
  P_LineAttack(player->mo, angle, range, slope, damage);

  // missed? ah, welp.
  if (!linetarget)
    return;

  // un-missed!
  S_StartSound(player->so, hitsound);

  // turn to face target
  player->mo->angle = R_PointToAngle2(player->mo->x, player->mo->y, linetarget->x, linetarget->y);
}

void A_WeaponSound(mobj_t *mobj, player_t *player, pspdef_t *psp)
{
	if (!player) return; // [crispy] let pspr action pointers get called from mobj states

	if (!psp->state)
		return;

	if (psp->state->args[1])
		// full volume
		// [crispy] prevent from adding up volume
		crispy->soundfull ? S_StartSoundOnce (NULL, psp->state->args[0]) : S_StartSound (NULL, psp->state->args[0]);
	else
		S_StartSound(player->so, psp->state->args[0]);
}

void A_ConsumeAmmo(mobj_t *mobj, player_t *player, pspdef_t *psp)
{
  int amount;
  ammotype_t ammonum;

  if (!player) return; // [crispy] let pspr action pointers get called from mobj states

  ammonum = weaponinfo[player->readyweapon].ammo;
  if (!psp->state || ammonum == am_noammo)
    return;

  // use the weapon's ammo-per-shot amount if zero.
  // to subtract zero ammo, don't call this function. ;)
  if (psp->state->args[0] != 0)
    amount = psp->state->args[0];
  else
    amount = weaponinfo[player->readyweapon].ammopershot;

  player->ammo[ammonum] -= amount;
  // [crispy] never allow less than zero ammo
  if (player->ammo[ammonum] < 0)
  {
    player->ammo[ammonum] = 0;
  }
}

void A_RefireTo(mobj_t *mobj, player_t *player, pspdef_t *psp)
{
  if (!player) return; // [crispy] let pspr action pointers get called from mobj states

  if (!psp->state)
    return;

  if ((psp->state->args[1] || P_CheckAmmo(player))
  &&  (player->cmd.buttons & BT_ATTACK)
  &&  (player->pendingweapon == wp_nochange && player->health))
    P_SetPsprite(player, ps_weapon, psp->state->args[0]);
}

void A_GunFlashTo(mobj_t *mobj, player_t *player, pspdef_t *psp)
{
  if (!player) return; // [crispy] let pspr action pointers get called from mobj states

  if (!psp->state)
    return;

  if(!psp->state->args[1])
    P_SetMobjState(player->mo, S_PLAY_ATK2);

  P_SetPsprite(player, ps_flash, psp->state->args[0]);
}
