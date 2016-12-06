/**
 * Copyright (c) 2016 Linagora
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
package org.ow2.petals.se.activiti.clientserver.api.monitoring;

import java.util.Map;

/**
 * Client/Server interface of the service 'Monitoring' part dedicated to the SE Activiti
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public interface MonitoringService
        extends org.ow2.petals.component.framework.clientserver.api.monitoring.MonitoringService {

    //
    // --- Process definitions / Process instances
    //

    /**
     * <p>
     * Returns information on process definitions.</>
     * <p>
     * The map key is the process definition identifier. Each value of the map entry is an array of numberes where:
     * <ul>
     * <li>the 1st value is the suspension state: {@code 1} the process definition is suspended, {@code 0} it is not
     * suspended,
     * <li>
     * <li>the 2nd value is the number of active process instances,</li>
     * <li>the 3rd value is the number of suspended process instances,</li>
     * <li>the 4th value is the number of ended process instances.</li>
     * </ul>
     * </p>
     */
    public Map<String, Long[]> getProcessDefinitions();

}
