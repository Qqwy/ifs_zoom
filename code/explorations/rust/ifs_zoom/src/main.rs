extern crate image;
extern crate rand;

use rand::Rng;

fn main() {
    println!("Hello, world!");
    let width = 1600;
    let height = 1600;


    let imgbuf = make_image2(width, height);
    imgbuf.save("example.png").unwrap();
}

fn make_image(width: u32, height: u32) -> image::RgbImage {
    let mut imgbuf = image::ImageBuffer::new(width, height);

    for(x, y, pixel) in imgbuf.enumerate_pixels_mut() {
        let r = 0.3 * x as f32;
        let b = 0.3 * y as f32;
        *pixel = image::Rgb([r as u8, 0, b as u8]);
    }
    imgbuf
}

fn make_image2(width: u32, height: u32) -> image::GrayImage {
    let mut imgbuf = image::GrayImage::new(width, height);
    let mut rng = rand::thread_rng();

    let mut x = 0.;
    let mut y = 0.;

    for _ in 0..(width * height) {
        let r = rng.gen::<f32>();
        let cx: f64;
        let cy: f64;

        if r <= 0.01 {
            cx = 0.0 as f64;
            cy = 0.16 * y as f64;
        } else if r <= 0.08 {
            cx = 0.2 * x as f64 - 0.26 * y as f64;
            cy = 0.23 * x as f64 + 0.22 * y as f64 + 1.6;
        } else if r <= 0.15 {
            cx = -0.15 * x as f64 + 0.28 * y as f64;
            cy = 0.26 * x as f64 + 0.26 * y as f64 + 0.44;
        } else {
            cx = 0.85 * x as f64 + 0.04 * y as f64;
            cy = -0.04 * x as f64 + 0.85 * y as f64 + 1.6;
        }
        x = cx;
        y = cy;

        let posx = ((width as f64) / 2. + x * (width as f64) / 11.).round() as i64;
        let posy = ((height as f64) - y * (height as f64) / 11.).round() as i64;
        if posx > 0 && posx < width as i64 && posy > 0 && posy < height as i64 {
            let pixel = imgbuf.get_pixel_mut(posx as u32, posy as u32);
            // if pixel[0] == 0 {
                pixel[0] = 255;
            // } else {
            //     pixel[0]  = pixel[0].saturating_sub(8);
            // };
            // // pixel[0] = pixel[0].saturating_add(10);
            // imgbuf.put_pixel(
            //     posx as u32,
            //     posy as u32,
            //     image::Luma([255])
            // );
        }

    };


    imgbuf
}
