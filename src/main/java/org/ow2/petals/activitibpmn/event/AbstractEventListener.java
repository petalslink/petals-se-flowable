/**
 * Copyright (c) 2015 Linagora
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
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.event;

import java.util.logging.Logger;

import org.activiti.engine.delegate.event.ActivitiEvent;
import org.activiti.engine.delegate.event.ActivitiEventListener;
import org.activiti.engine.delegate.event.ActivitiEventType;

/**
 * Abstract class managing Activiti event
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public abstract class AbstractEventListener implements ActivitiEventListener {

    protected final Logger log;

    private final ActivitiEventType listenEventType;

    public AbstractEventListener(final ActivitiEventType listenEventType, final Logger log) {

        assert log != null;
        assert listenEventType != null;

        this.log = log;
        this.listenEventType = listenEventType;
    }

    protected abstract void processEvent(final ActivitiEvent event);

    @Override
    public void onEvent(final ActivitiEvent event) {

        assert event != null;

        if (event.getType() == this.listenEventType) {
            
            this.processEvent(event);
            
        } else {
            log.warning("Unexpected event type '" + event.getType().name() + "'. Event discarded !");
        }

    }

    @Override
    public boolean isFailOnException() {
        return false;
    }

    public ActivitiEventType getListenEventType() {
        return listenEventType;
    }

}
