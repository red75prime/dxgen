use cgmath::*;
use utils::*;

#[derive(Debug, Clone)]
pub struct Camera {
    pub eye: Point3<f32>,
    pub fwd: Vector3<f32>,
    pub up: Vector3<f32>,
    pub right: Vector3<f32>,
    pub rot: Quaternion<f32>,
    vfov_deg: f32,
    aspect: f32,
    pub near: f32,
    pub far: f32,
}

impl Camera {
    pub fn new() -> Camera {
        Camera {
            eye: p3(0., 0., 0.),
            fwd: v3(0., 0., -1.),
            up: v3(0., 1., 0.),
            right: v3(1., 0., 0.),
            rot: Quaternion::one(),
            vfov_deg: 60.,
            aspect: 1.,
            near: 0.1,
            far: 10000.,
        }
    }

    pub fn rotx(&mut self, ang: f32) {
        let rotm = Quaternion::from_axis_angle(self.up, Deg(ang));
        self.rot = rotm * self.rot;
        self.update_axes(); //TODO: optimize, when it becomes not early optimization.
    }

    pub fn roty(&mut self, ang: f32) {
        let rotm = Quaternion::from_axis_angle(self.right, Deg(ang));
        self.rot = rotm * self.rot;
        self.update_axes();
    }

    pub fn rotz(&mut self, ang: f32) {
        let rotm = Quaternion::from_axis_angle(self.fwd, Deg(ang));
        self.rot = rotm * self.rot;
        self.update_axes();
    }

    /// rotate camera by at most deg degrees around fwd axis to make up axis point at +y direction
    pub fn restore_up(&mut self, deg: f32) {
        // if fwd points more than 60 deg up or down, then skip this
        let fwd_y_proj = self.fwd.dot(v3(0., 1., 0.));
        if fwd_y_proj < -0.87 || fwd_y_proj > 0.87 {
            return;
        }
        // project y axis onto up/right plane
        let pi = ::std::f32::consts::PI;
        let x = self.up.dot(v3(0., 1., 0.));
        let y = self.right.dot(v3(0., 1., 0.));
        let ang = y.atan2(x)/pi*180.;
        let ang = if ang > deg { deg } else if ang < -deg { -deg } else { ang };
        self.rotz(ang);
    }

    fn update_axes(&mut self) {
        self.fwd = self.rot.rotate_vector(v3(0., 0., -1.));
        self.right = self.rot.rotate_vector(v3(1., 0., 0.));
        self.up = self.rot.rotate_vector(v3(0., 1., 0.));
    }

    pub fn go(&mut self, fwd: f32, right: f32, up: f32) {
        self.eye = self.eye + self.fwd * fwd + self.right * right + self.up * up;
    }

    pub fn view_matrix(&self) -> Matrix4<f32> {
        Matrix4::look_at(self.eye, self.eye + self.fwd, self.up)
    }

    pub fn projection_matrix(&self) -> Matrix4<f32> {
        perspective(Deg(self.vfov_deg), self.aspect, self.near, self.far)
    }

    pub fn set_aspect(&mut self, aspect: f32) {
        self.aspect = aspect;
    }

    pub fn fov(&self) -> f32 {
        self.vfov_deg
    }

    pub fn set_fov(&mut self, fovy_deg: f32) {
        self.vfov_deg = fovy_deg;
    }

    pub fn tn_half_fov(&self) -> [f32; 2] {
        [f32::tan(self.vfov_deg.to_radians()/2.)*self.aspect, f32::tan(self.vfov_deg.to_radians()/2.)]
    }
}
