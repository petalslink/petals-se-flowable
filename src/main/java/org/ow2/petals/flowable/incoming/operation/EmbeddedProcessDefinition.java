/**
 * Copyright (c) 2015-2024 Linagora
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
package org.ow2.petals.flowable.incoming.operation;

import org.flowable.bpmn.model.BpmnModel;

/**
 * A process definition embedded into the service unit
 * 
 * @author Bertrand ESCUDIE - Linagora
 * @author Christophe DENEUX - Linagora
 * 
 */
public class EmbeddedProcessDefinition {

    /**
     * The file name of the embedded process
     */
    private final String processFileName;

    /**
     * The version of the embedded process
     */
    private final int version;
    
    /**
     * The model of the embedded process
     */
    private BpmnModel model;

    public EmbeddedProcessDefinition(final String processFileName, final int version, final BpmnModel model) {
		this.processFileName = processFileName;
		this.version = version;
		this.model = model;
	}

    /**
     * @return The file name of the embedded process
     */
    public final String getProcessFileName() {
        return this.processFileName;
    }

    /**
     * @return The version of the embedded process
     */
    public final int getVersion() {
        return this.version;
    }

    /**
     * @return The model of the embedded process
     */
    public final BpmnModel getModel() {
        return this.model;
    }
}