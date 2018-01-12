
#[derive(Clone, Copy, Debug)]
pub struct Rect {
    l: f32,
    r: f32,
    t: f32,
    b: f32,
}

pub trait Graphics {
    type FontDesc;
    type RichText;
    type LineDesc;
    type FillDesc;

    fn set_origin(x: f32, y: f32);
    fn origin() -> (f32, f32);
    fn set_clipping(r: Rect);
    fn draw_rect(r: Rect, l: &Self::LineDesc);
    fn fill_rect(r: Rect, f: &Self::FillDesc);
    fn draw_line(r: Rect, l: &Self::LineDesc);
    fn draw_text(r: Rect, t: &str, f: &Self::FontDesc);
    fn draw_rich_text(r: Rect, t: &Self::RichText);
}

