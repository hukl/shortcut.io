const puppeteer = require('puppeteer');

async function run() {
    const browser = await puppeteer.launch({
        defaultViewport: {
            width:  1024,
            height: 1024
        }
    });
    const page    = await browser.newPage();

    await page.goto(process.argv[3]);
    await page.waitFor(500);
    await page.screenshot({
        path: process.argv[2] + '.jpg',
        quality: 80
    });

    browser.close();
}

run();
