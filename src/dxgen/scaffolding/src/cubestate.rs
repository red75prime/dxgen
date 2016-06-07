use cgmath::*;
use crossbeam;
use rand;
use rand::Rng;
use utils::v3;
use ncollide as nc;
use ncollide::broad_phase::BroadPhase;
use ncollide::bounding_volume;
use nalgebra as na;

pub type V3 = Vector3<f32>;

#[derive(Clone)]
pub struct CubeState {
    pub pos: V3,
    pub rot: Basis3<f32>,
    pub spd: V3,
    pub color: V3,
    pub blink: bool,
    pub rot_axe: V3,
    pub rot_spd: f32,
}

#[derive(Clone)]
pub struct State {
    pub tick: f64,
    pub cubes: Vec<CubeState>,
    pub light_pos: V3,
}

const CUBE_SPAN : f32 = 200.;

fn fmin(a: (f32, f32), b: (f32, f32)) -> (f32, f32) {
    if a.0 < b.0 { a } else { b }
}

impl State {
    pub fn new(object_count: u32) -> Self {
        let mut rng = rand::XorShiftRng::new_unseeded();
        let mut cubes = Vec::with_capacity(object_count as usize);
        for _ in 0 .. object_count {
            let x = rng.next_f32();
            let y = rng.next_f32();
            let z = rng.next_f32();
            let cube_state = CubeState {
                pos: v3((x - 0.5) * CUBE_SPAN,
                             (y - 0.5) * CUBE_SPAN,
                             (z - 1.) * CUBE_SPAN),
                color: v3(x, y, z),
                blink: false,
                rot: <Basis3<f32> as Rotation<_>>::one(),
                spd: v3(rng.next_f32() - 0.5,
                             rng.next_f32() - 0.5,
                             rng.next_f32() - 0.5).normalize()*rng.next_f32()*1.,
                rot_axe: v3(rng.next_f32() - 0.5,
                             rng.next_f32() - 0.5,
                             rng.next_f32() - 0.5)
                              .normalize(),
                rot_spd: rng.next_f32()*30.0,
            };
            cubes.push(cube_state);
        };
        State {
            tick: 0.0,
            cubes: cubes,
            light_pos: v3(0., 0., 0.),
        }
    }

    fn update_one(cube: &CubeState, dt: f32) -> CubeState {
        let a = v3(0., -9.8, 0.);
        let mut new_spd = cube.spd + a*dt;
        let dp = cube.spd * dt + a * dt * dt / 2.;
        let mut new_pos = cube.pos + dp;
        let mut bounce = false;
        let (dist, sprj) = fmin(((new_pos.x - (-CUBE_SPAN/2.)).abs(), new_spd.x), fmin(((new_pos.x - CUBE_SPAN/2.).abs(), new_spd.x), 
                    fmin(((new_pos.y - (-CUBE_SPAN/2.)).abs(), new_spd.y), fmin(((new_pos.y - (CUBE_SPAN/2.)).abs(), new_spd.y), 
                    fmin(((new_pos.z - (-CUBE_SPAN)).abs(), new_spd.z), ((new_pos.z - 0.).abs(), new_spd.z))))));
        if new_pos.x < -CUBE_SPAN/2. {
            new_pos.x = -CUBE_SPAN - new_pos.x;
            new_spd.x = -new_spd.x;
            bounce = true;
        }
        if new_pos.x > CUBE_SPAN/2. {
            new_pos.x = CUBE_SPAN - new_pos.x;
            new_spd.x = -new_spd.x;
            bounce = true;
        }
        if new_pos.y < -CUBE_SPAN/2. {
            new_pos.y = -CUBE_SPAN - new_pos.y;
            new_spd.y = -new_spd.y;
            bounce = true;
        }
        if new_pos.y > CUBE_SPAN/2. {
            new_pos.y = CUBE_SPAN - new_pos.y;
            new_spd.y = -new_spd.y;
            bounce = true;
        }
        if new_pos.z < -CUBE_SPAN {
            new_pos.z = -CUBE_SPAN*2. - new_pos.z;
            new_spd.z = -new_spd.z;
            bounce = true;
        }
        if new_pos.z > 0. {
            new_pos.z = -new_pos.z;
            new_spd.z = -new_spd.z;
            bounce = true;
        }
        if bounce { new_spd *= 0.95 };
        if bounce && new_spd.magnitude2() < 0.8 {
            new_spd = v3(0., 0., 0.);
        }
        CubeState {
            pos: new_pos,
            spd: new_spd,
            rot: Basis3::from_axis_angle(cube.rot_axe, deg(cube.rot_spd * dt).into()).concat(&cube.rot),
            blink: dist.abs() < sprj.abs()/4. && new_spd.magnitude2()>4.,
            .. *cube
        }
    }

    pub fn update(&self, dt: f32, thread_cnt: u32) -> Self {
        if dt == 0.0 {
          return (*self).clone();
        }
        // advance position of cubes
        let mut cubes = Vec::with_capacity(self.cubes.len());
        if thread_cnt == 1 {
            for cube in &self.cubes[..] {
                cubes.push(State::update_one(cube, dt));
            }
        } else {
            // Uninitialized content of cubes will be immediately overwritten
            unsafe{ cubes.set_len(self.cubes.len()) };
            crossbeam::scope(|scope| {
                let chunk_len = (cubes.len()/thread_cnt as usize)+1;
                for (chunk, chunk_src) in cubes[..].chunks_mut(chunk_len).zip(self.cubes[..].chunks(chunk_len)) {
                    scope.spawn(move || {
                        for (c, cube) in chunk.iter_mut().zip(chunk_src) {
                            *c = State::update_one(cube, dt);
                        };
                    });
                };
            });
        }
        let mut broad_phase = nc::broad_phase::DBVTBroadPhase::new(0.1f32, true);
        for (i, c) in self.cubes.iter().enumerate() {
            let bv = bounding_volume::bounding_sphere(&nc::shape::Cuboid::new(na::Vector3::new(0.5, 0.5, 0.5)), &na::Vector3::new(c.pos.x, c.pos.y, c.pos.z));
            //println!("{:?}", bv);
            broad_phase.deferred_add(i, bv, i);
        }
        let mut ccount = 0;
        broad_phase.update(&mut |&i1, &i2| {(self.cubes[i1].pos - self.cubes[i2].pos).magnitude2() < 4.0}, &mut |&i1, &i2, c|{
            if c {
                let dp = (cubes[i2].pos - cubes[i1].pos).normalize();
                let ds = (cubes[i2].spd - cubes[i1].spd);
                let ci_spd = ds.dot(dp); 
                if ci_spd < 0. {
                    // closing in
                    cubes[i1].spd += dp*(ci_spd/2.01);
                    cubes[i1].rot_spd /= 2.;
                    cubes[i2].spd -= dp*(ci_spd/2.01);
                    cubes[i2].rot_spd /= 2.;
                }
            }
            //ccount += 1;
        });
        //println!("Comparisons: {}", ccount);

        let (lx, ly) = f64::sin_cos(self.tick*0.05);
        let (lx, ly) = (lx as f32, ly as f32);
        let light_pos = v3(lx*50., ly*50., -0.);//v3(0., 0., -100.);
        State {
            tick: self.tick + dt as f64,
            cubes: cubes,
            light_pos: light_pos,
        }
    }
}

use std::thread;
use std::sync::Arc;
use std::sync::mpsc::{channel, Receiver, Sender};

fn state_update_agent(thread_cnt: u32, rx: Receiver<(Arc<State>, f32)>, tx: Sender<State>) {
    loop {
        if let Ok((state_ref, dt)) = rx.recv() {
            if let Err(_) = tx.send(state_ref.update(dt, thread_cnt)) {
                return ();
            }
        } else {
            return ();
        }
    }
}

pub struct StateUpdateAgent {
    tx: Sender<(Arc<State>, f32)>,
    rx: Receiver<State>,
}

impl StateUpdateAgent {
    pub fn new(thread_cnt: u32) -> Self {
        let (otx, orx) = channel();
        let (itx, irx) = channel();
        thread::spawn(move || state_update_agent(thread_cnt, orx, itx));
        StateUpdateAgent {
            tx: otx,
            rx: irx,
        }
    }

    pub fn start_update<'a>(&'a self, state: Arc<State>, dt: f32) -> UpdateResult<'a> {
        self.tx.send((state, dt)).unwrap();
        UpdateResult{
            agent: self,
            result_aquired: false,
        }
    }
}

pub struct UpdateResult<'a> {
    agent: &'a StateUpdateAgent,
    result_aquired: bool,
}

impl<'a> UpdateResult<'a> {
    pub fn get(&mut self) -> State {
        self.result_aquired = true;
        self.agent.rx.recv().unwrap()
    }
}

impl<'a> Drop for UpdateResult<'a> {
    fn drop(&mut self) {
        if !self.result_aquired {
            error!("Update result is not retrieved before drop");
        }
    }
}
