/**
 * Copyright (c) 2018 Linagora
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
package org.ow2.petals.se.flowable.clientserver.api.admin;

import java.util.List;

import org.ow2.petals.basisapi.exception.PetalsException;
import org.ow2.petals.se.flowable.clientserver.api.admin.exception.ProcessInstanceNotFoundException;

/**
 * Client/Server interface of the service 'Runtime Administration' part dedicated to the SE Flowable when it is running
 * (in state 'Started').
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public interface AdminRuntimeService {

    /**
     * <p>
     * List process instances that can be purged for a given process definition in its given version. A process instance
     * can be purged if it is ended successfully or with error.
     * </p>
     * <p>
     * If no process definition is given (key or version are null or empty), all purgeable process instances of all
     * process definitions are returned.
     * </p>
     * 
     * @param processDefinitionKey
     *            The process definition of process instances to list. Can be {@link null} or empty.
     * @param processDefinitionVersion
     *            Version of the process definition of process instances to list. Can be {@link null} or empty.
     * @return List of purgeable process instance identifiers.
     * @throws PetalsException
     *             An error occurs listing process instances.
     */
    public List<String> listPurgeableProcessInstances(final String processDefinitionKey,
            final int processDefinitionVersion) throws PetalsException;

    /**
     * Purge a list of process instances deleting all data from each process instances.
     * 
     * @param procInstId
     *            Identifier of the process instance to purge. Not {@code null} and not empty.
     * @param returns
     *            correlated flow instances
     * @return List of flow instance identifiers associated to purged process instances, to use to purge MONIT logs for
     *         example
     * @throws ProcessInstanceNotFoundException
     *             The given process instance does not exist.
     * @throws PetalsException
     *             An error occurs purging process instances. Some process instances can be not purged.
     */
    public List<String> purgeProcessInstance(final String procInstId, final boolean returnsCorrelatedFlows)
            throws PetalsException;

}
