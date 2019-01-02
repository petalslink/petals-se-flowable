/**
 * Copyright (c) 2015-2019 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * A {@link ThreadFactory} creating threads in charge of logging delayed MONIT traces
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class ScheduledLoggerThreadFactory implements ThreadFactory {

    private final AtomicInteger threadNumber = new AtomicInteger(1);

    private final String componentName;

    public ScheduledLoggerThreadFactory(final String componentName) {
        this.componentName = componentName;
    }

    @Override
    public Thread newThread(final Runnable r) {
        return new Thread(r, this.componentName + " - Scheduled logger #"
                + Integer.toString(threadNumber.getAndIncrement()));
    }

}
